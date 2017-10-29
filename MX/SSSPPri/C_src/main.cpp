// Copyright (C) 2013-2016 Altera Corporation, San Jose, California, USA. All rights reserved.
// Permission is hereby granted, free of charge, to any person obtaining a copy of this
// software and associated documentation files (the "Software"), to deal in the Software
// without restriction, including without limitation the rights to use, copy, modify, merge,
// publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to
// whom the Software is furnished to do so, subject to the following conditions:
// The above copyright notice and this permission notice shall be included in all copies or
// substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
// HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
// WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
// OTHER DEALINGS IN THE SOFTWARE.
// 
// This agreement shall be governed in all respects by the laws of the State of California and
// by the laws of the United States of America.

///////////////////////////////////////////////////////////////////////////////////
// This host program executes a vector addition kernel to perform:
//  C = A + B
// where A, B and C are vectors with N elements.
//
// This host program supports partitioning the problem across multiple OpenCL
// devices if available. If there are M available devices, the problem is
// divided so that each device operates on N/M points. The host program
// assumes that all devices are of the same type (that is, the same binary can
// be used), but the code can be generalized to support different device types
// easily.
//
// Verification is performed against the same computation on the host CPU.
///////////////////////////////////////////////////////////////////////////////////

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "CL/opencl.h"
#include "AOCLUtils/aocl_utils.h"

#include "sssp.h"

#include <set>

using namespace aocl_utils;

// OpenCL runtime configuration
cl_platform_id platform = NULL;
unsigned num_devices = 0;
scoped_array<cl_device_id> device; // num_devices elements
cl_context context = NULL;
scoped_array<cl_command_queue> queue; // num_devices elements
cl_program program = NULL;
scoped_array<cl_kernel> kernel; // num_devices elements
#if USE_SVM_API == 0
scoped_array<cl_mem> d_indices; // num_devices elements
scoped_array<cl_mem> d_neighbors; // num_devices elements
scoped_array<cl_mem> d_weights; // num_devices elements
scoped_array<cl_mem> d_worklist1; // num_devices elements
scoped_array<cl_mem> d_worklist2; // num_devices elements
scoped_array<cl_mem> d_results; // num_devices elements
#endif /* USE_SVM_API == 0 */

// Problem data.
unsigned N = 1024; // problem size
unsigned E = 50*N;
#if USE_SVM_API == 0
scoped_array<scoped_aligned_ptr<int> > h_indices, h_neighbors, h_weights; // num_devices elements
scoped_array<scoped_aligned_ptr<activeWork> > h_worklist1; // num_devices elements
scoped_array<scoped_aligned_ptr<int> > h_results; // num_devices elements
#else
scoped_array<scoped_SVM_aligned_ptr<float> > h_indices, h_neighbors; // num_devices elements
scoped_array<scoped_SVM_aligned_ptr<float> > h_results; // num_devices elements
#endif /* USE_SVM_API == 0 */
scoped_array<scoped_array<int> > ref_h_results; // num_devices elements
scoped_array<unsigned> v_per_device; // num_devices elements
scoped_array<unsigned> max_e_per_device; // num_devices elements

// Function prototypes
float rand_float();
bool init_opencl();
void init_problem();
void run();
void cleanup();

// Entry point.
int main(int argc, char **argv) {
  Options options(argc, argv);

  // Optional argument to specify the problem size.
  if(options.has("n")) {
    N = options.get<unsigned>("n");
    E = 50*N;
  }

  // Initialize OpenCL.
  if(!init_opencl()) {
    return -1;
  }

  // Initialize the problem data.
  // Requires the number of devices to be known.
  init_problem();

  // Run the kernel.
  run();

  // Free the resources allocated
  cleanup();

  return 0;
}

/////// HELPER FUNCTIONS ///////

// Randomly generate a floating-point number between -10 and 10.
int rand_int() {
  // return float(rand()) / float(RAND_MAX) * 20.0f - 10.0f;
  return rand();
}

float rand_float() {
  return float(rand()) / float(RAND_MAX) * 20.0f - 10.0f;
}

// Initializes the OpenCL objects.
bool init_opencl() {
  cl_int status;

  printf("Initializing OpenCL\n");

  if(!setCwdToExeDir()) {
    return false;
  }

  // Get the OpenCL platform.
  platform = findPlatform("Altera");
  if(platform == NULL) {
    printf("ERROR: Unable to find Intel(R) FPGA OpenCL platform.\n");
    return false;
  }

  // Query the available OpenCL device.
  device.reset(getDevices(platform, CL_DEVICE_TYPE_ALL, &num_devices));
  printf("Platform: %s\n", getPlatformName(platform).c_str());
  printf("Using %d device(s)\n", num_devices);
  for(unsigned i = 0; i < num_devices; ++i) {
    printf("  %s\n", getDeviceName(device[i]).c_str());
  }

  // Create the context.
  context = clCreateContext(NULL, num_devices, device, &oclContextCallback, NULL, &status);
  checkError(status, "Failed to create context");

  // Create the program for all device. Use the first device as the
  // representative device (assuming all device are of the same type).
  std::string binary_file = getBoardBinaryFile("sssp", device[0]);
  printf("Using AOCX: %s\n", binary_file.c_str());
  program = createProgramFromBinary(context, binary_file.c_str(), device, num_devices);

  // Build the program that was just created.
  status = clBuildProgram(program, 0, NULL, "", NULL, NULL);
  checkError(status, "Failed to build program");

  // Create per-device objects.
  queue.reset(num_devices);
  kernel.reset(num_devices);
  v_per_device.reset(num_devices);
  max_e_per_device.reset(num_devices);
#if USE_SVM_API == 0
  d_indices.reset(num_devices);
  d_neighbors.reset(num_devices);
  d_weights.reset(num_devices);
  d_worklist1.reset(num_devices);
  d_worklist2.reset(num_devices);
  d_results.reset(num_devices);
#endif /* USE_SVM_API == 0 */

  for(unsigned i = 0; i < num_devices; ++i) {
    // Command queue.
    queue[i] = clCreateCommandQueue(context, device[i], CL_QUEUE_PROFILING_ENABLE, &status);
    checkError(status, "Failed to create command queue");

    // Kernel.
    const char *kernel_name = "sssp";
    kernel[i] = clCreateKernel(program, kernel_name, &status);
    checkError(status, "Failed to create kernel");

    // Determine the number of elements processed by this device.
    v_per_device[i] = N / num_devices; // number of elements handled by this device
    max_e_per_device[i] = E / num_devices;

    // Spread out the remainder of the elements over the first
    // N % num_devices.
    if(i < (N % num_devices)) {
      v_per_device[i]++;
    }
    if(i < (E % num_devices)) {
      max_e_per_device[i]++;
    }

#if USE_SVM_API == 0
    // Input buffers.
    d_indices[i] = clCreateBuffer(context, CL_MEM_READ_ONLY, 
        (v_per_device[i]+1) * sizeof(int), NULL, &status);
    checkError(status, "Failed to create buffer for d_indices");

    d_neighbors[i] = clCreateBuffer(context, CL_MEM_READ_ONLY, 
        max_e_per_device[i] * sizeof(int), NULL, &status);
    checkError(status, "Failed to create buffer for d_d_weights");

    d_weights[i] = clCreateBuffer(context, CL_MEM_READ_ONLY, 
        max_e_per_device[i] * sizeof(int), NULL, &status);
    checkError(status, "Failed to create buffer for d_weights");

    // Inout buffers
    d_worklist1[i] = clCreateBuffer(context, CL_MEM_READ_WRITE, 
        10 * v_per_device[i] * sizeof(activeWork), NULL, &status);
    checkError(status, "Failed to create buffer for d_worklist1");

    d_worklist2[i] = clCreateBuffer(context, CL_MEM_READ_WRITE, 
        10 * v_per_device[i] * sizeof(activeWork), NULL, &status);
    checkError(status, "Failed to create buffer for d_worklist2");

    // Output buffer.
    d_results[i] = clCreateBuffer(context, CL_MEM_READ_WRITE, 
        v_per_device[i] * sizeof(int), NULL, &status);
    checkError(status, "Failed to create buffer for d_results");
#else
    cl_device_svm_capabilities caps = 0;

    status = clGetDeviceInfo(
      device[i],
      CL_DEVICE_SVM_CAPABILITIES,
      sizeof(cl_device_svm_capabilities),
      &caps,
      0
    );
    checkError(status, "Failed to get device info");

    if (!(caps & CL_DEVICE_SVM_COARSE_GRAIN_BUFFER)) {
      printf("The host was compiled with USE_SVM_API, however the device currently being targeted does not support SVM.\n");
      // Free the resources allocated
      cleanup();
      return false;
    }
#endif /* USE_SVM_API == 0 */
  }

  return true;
}

// Initialize the data for the problem. Requires num_devices to be known.
void init_problem() {
  if(num_devices == 0) {
    checkError(-1, "No devices");
  }

  h_indices.reset(num_devices);
  h_neighbors.reset(num_devices);
  h_weights.reset(num_devices);
  h_worklist1.reset(num_devices);
  h_results.reset(num_devices);
  ref_h_results.reset(num_devices);

  // Generate input vectors A and B and the reference output consisting
  // of a total of N elements.
  // We create separate arrays for each device so that each device has an
  // aligned buffer.
  for(unsigned i = 0; i < num_devices; ++i) {
#if USE_SVM_API == 0
    h_indices[i].reset(v_per_device[i]+1);
    h_neighbors[i].reset(max_e_per_device[i]);
    h_weights[i].reset(max_e_per_device[i]);
    h_worklist1[i].reset(10 * v_per_device[i]);
    h_results[i].reset(v_per_device[i]);
    ref_h_results[i].reset(v_per_device[i]);

printf("Num of Nodes: %d, Num of Edges: %d\n", N, E);

    int count = 0;
    std::set<int> neighbor_record;
    for (int j = 0; j < N; j++) {
      h_indices[i][j] = count;
      if (count < max_e_per_device[i]) {
        while ((rand() % 100) > 10) {  // degree = 10
          int neighbor = rand() % N;
          if (neighbor == j || (neighbor_record.find(neighbor) != neighbor_record.end())) continue;
          neighbor_record.insert(neighbor);
          h_neighbors[i][count] = neighbor;
          h_weights[i][count] = rand() % 1000;
          count++;
          if (count == max_e_per_device[i]) {  // full
            break;
          }
        }
        neighbor_record.clear();
      }
    }
    h_indices[i][N] = count;

    h_results[i][0] = 0;
    for (int j = 1; j < v_per_device[i]; j++) {
      h_results[i][j] = RAND_MAX;
    }

    h_worklist1[i][0] = {0, 0};

#else
    h_indices[i].reset(context, v_per_device[i]);
    h_neighbors[i].reset(context, v_per_device[i]);
    h_results[i].reset(context, v_per_device[i]);
    ref_h_results[i].reset(v_per_device[i]);

    cl_int status;

    status = clEnqueueSVMMap(queue[i], CL_TRUE, CL_MAP_WRITE,
        (void *)h_indices[i], v_per_device[i] * sizeof(float), 0, NULL, NULL);
    checkError(status, "Failed to map input A");
    status = clEnqueueSVMMap(queue[i], CL_TRUE, CL_MAP_WRITE,
        (void *)h_neighbors[i], v_per_device[i] * sizeof(float), 0, NULL, NULL);
    checkError(status, "Failed to map input B");

    for(unsigned j = 0; j < v_per_device[i]; ++j) {
      h_indices[i][j] = rand_float();
      h_neighbors[i][j] = rand_float();
      ref_h_results[i][j] = h_indices[i][j] + h_neighbors[i][j];
    }

    status = clEnqueueSVMUnmap(queue[i], (void *)h_indices[i], 0, NULL, NULL);
    checkError(status, "Failed to unmap input A");
    status = clEnqueueSVMUnmap(queue[i], (void *)h_neighbors[i], 0, NULL, NULL);
    checkError(status, "Failed to unmap input B");
#endif /* USE_SVM_API == 0 */
  }
}

void run() {
  cl_int status;

  const double start_time = getCurrentTimestamp();

  // Launch the problem for each device.
  scoped_array<cl_event> kernel_event(num_devices);
  scoped_array<cl_event> finish_event(num_devices);

  for(unsigned i = 0; i < num_devices; ++i) {

#if USE_SVM_API == 0
    // Transfer inputs to each device. Each of the host buffers supplied to
    // clEnqueueWriteBuffer here is already aligned to ensure that DMA is used
    // for the host-to-device transfer.
    cl_event write_event[5];
    status = clEnqueueWriteBuffer(queue[i], d_indices[i], CL_FALSE,
        0, (v_per_device[i]+1) * sizeof(int), h_indices[i], 0, NULL, &write_event[0]);
    checkError(status, "Failed to transfer d_indices");

    status = clEnqueueWriteBuffer(queue[i], d_neighbors[i], CL_FALSE,
        0, max_e_per_device[i] * sizeof(int), h_neighbors[i], 0, NULL, &write_event[1]);
    checkError(status, "Failed to transfer d_neighbors");

    status = clEnqueueWriteBuffer(queue[i], d_weights[i], CL_FALSE,
        0, max_e_per_device[i] * sizeof(int), h_weights[i], 0, NULL, &write_event[2]);
    checkError(status, "Failed to transfer d_weights");

    status = clEnqueueWriteBuffer(queue[i], d_worklist1[i], CL_FALSE,
        0, sizeof(activeWork), h_worklist1[i], 0, NULL, &write_event[3]);
    checkError(status, "Failed to transfer d_worklist1");

    status = clEnqueueWriteBuffer(queue[i], d_results[i], CL_FALSE,
        0, v_per_device[i] * sizeof(int), h_results[i], 0, NULL, &write_event[4]);
    checkError(status, "Failed to transfer d_result");
#endif /* USE_SVM_API == 0 */

    // Set kernel arguments.
    unsigned argi = 0;

#if USE_SVM_API == 0
    status = clSetKernelArg(kernel[i], argi++, sizeof(cl_mem), &d_indices[i]);
    checkError(status, "Failed to set argument %d", argi - 1);

    status = clSetKernelArg(kernel[i], argi++, sizeof(cl_mem), &d_neighbors[i]);
    checkError(status, "Failed to set argument %d", argi - 1);

    status = clSetKernelArg(kernel[i], argi++, sizeof(cl_mem), &d_weights[i]);
    checkError(status, "Failed to set argument %d", argi - 1);

    status = clSetKernelArg(kernel[i], argi++, sizeof(cl_mem), &d_worklist1[i]);
    checkError(status, "Failed to set argument %d", argi - 1);

    status = clSetKernelArg(kernel[i], argi++, sizeof(cl_mem), &d_worklist2[i]);
    checkError(status, "Failed to set argument %d", argi - 1);

    status = clSetKernelArg(kernel[i], argi++, sizeof(cl_mem), &d_results[i]);
    checkError(status, "Failed to set argument %d", argi - 1);
#else
    status = clSetKernelArgSVMPointer(kernel[i], argi++, (void*)h_indices[i]);
    checkError(status, "Failed to set argument %d", argi - 1);

    status = clSetKernelArgSVMPointer(kernel[i], argi++, (void*)h_neighbors[i]);
    checkError(status, "Failed to set argument %d", argi - 1);

    status = clSetKernelArgSVMPointer(kernel[i], argi++, (void*)h_results[i]);
    checkError(status, "Failed to set argument %d", argi - 1);
#endif /* USE_SVM_API == 0 */

    // Enqueue kernel.
    // Use a global work size corresponding to the number of elements to add
    // for this device.
    //
    // We don't specify a local work size and let the runtime choose
    // (it'll choose to use one work-group with the same size as the global
    // work-size).
    //
    // Events are used to ensure that the kernel is not launched until
    // the writes to the input buffers have completed.
    const size_t global_work_size = 1;
    const size_t local_work_size = 1;
    printf("Launching for device %d (%zd elements)\n", i, global_work_size);

#if USE_SVM_API == 0
    status = clEnqueueNDRangeKernel(queue[i], kernel[i], 1, NULL,
        &global_work_size, NULL, 5, write_event, &kernel_event[i]);
#else
    status = clEnqueueNDRangeKernel(queue[i], kernel[i], 1, NULL,
        &global_work_size, &local_work_size, 0, NULL, &kernel_event[i]);
#endif /* USE_SVM_API == 0 */
    checkError(status, "Failed to launch kernel");

#if USE_SVM_API == 0
    // Read the result. This the final operation.
    status = clEnqueueReadBuffer(queue[i], d_results[i], CL_FALSE,
        0, v_per_device[i] * sizeof(int), h_results[i], 1, &kernel_event[i], &finish_event[i]);

    // Release local events.
    clReleaseEvent(write_event[0]);
    clReleaseEvent(write_event[1]);
    clReleaseEvent(write_event[2]);
    clReleaseEvent(write_event[3]);
#else
    status = clEnqueueSVMMap(queue[i], CL_TRUE, CL_MAP_READ,
        (void *)h_results[i], v_per_device[i] * sizeof(float), 0, NULL, NULL);
    checkError(status, "Failed to map h_results");
	clFinish(queue[i]);
#endif /* USE_SVM_API == 0 */
  }

  // Wait for all devices to finish.
  clWaitForEvents(num_devices, finish_event);

  const double end_time = getCurrentTimestamp();

  // Wall-clock time taken.
  printf("\nTime: %0.3f ms\n", (end_time - start_time) * 1e3);

  // Get kernel times using the OpenCL event profiling API.
  for(unsigned i = 0; i < num_devices; ++i) {
    cl_ulong time_ns = getStartEndTime(kernel_event[i]);
    printf("Kernel time (device %d): %0.3f ms\n", i, double(time_ns) * 1e-6);
  }

  // Release all events.
  for(unsigned i = 0; i < num_devices; ++i) {
    clReleaseEvent(kernel_event[i]);
    clReleaseEvent(finish_event[i]);
  }

  // Verify results.
  const double serialTimeStart = getCurrentTimestamp();
  bool pass = true;
  std::map<int, int> worklist;
  ref_h_results[0][0] = 0;
  for (int i = 1; i < N; i++) {
    ref_h_results[0][i] = RAND_MAX;
  }
  worklist.insert(std::pair<int, int>(0, 0));
  
  while (!worklist.empty()) {
    std::map<int, int>::iterator it = worklist.begin();
    int node = it->first;
    int dist = it->second;
    worklist.erase(it);
    for (int j = h_indices[0][node]; j < h_indices[0][node+1]; j++) {
      int newDist = dist + h_weights[0][j];
      int neighbor = h_neighbors[0][j];
      int neighbor_dist = ref_h_results[0][neighbor];
      if (newDist < neighbor_dist) {
        ref_h_results[0][neighbor] = newDist;
        worklist[neighbor] = newDist;
      }
    }
  }
  const double serialTimeEnd = getCurrentTimestamp();
  printf("\nSerial Time: %0.3f ms\n", (serialTimeEnd - serialTimeStart) * 1e3);

  for (int i = 0; i < N; i++) {
    if (ref_h_results[0][i] != h_results[0][i]) {
      printf ("inconsistency at node %d, should be %d, got %d\n", i, ref_h_results[0][i], h_results[0][i]);
      pass = false;
      break;
    }
  }
/*
  printf ("First 30 elements of results are:\n");
  for (int j = 0; j < 30 && j < N; j++) {
    printf ("%d ", h_results[0][j]);
  }
  printf ("\n");

  printf ("First 30 elements of ref results are:\n");
  for (int j = 0; j < 30 && j < N; j++) {
    printf ("%d ", ref_h_results[0][j]);
  }
  printf ("\n");

  printf ("First 30 elements of indices are:\n");
  for (int j = 0; j < 30 && j < N+1; j++) {
    printf ("%d ", h_indices[0][j]);
  }
  printf ("\n");

  printf ("First 30 elements of neighbors are:\n");
  for (int j = 0; j < 30; j++) {
    printf ("%d ", h_neighbors[0][j]);
  }
  printf ("\n");

  printf ("First 30 elements of weights are:\n");
  for (int j = 0; j < 30; j++) {
    printf ("%d ", h_weights[0][j]);
  }
  printf ("\n");*/


#if USE_SVM_API == 1
  for (unsigned i = 0; i < num_devices; ++i) {
    status = clEnqueueSVMUnmap(queue[i], (void *)h_results[i], 0, NULL, NULL);
    checkError(status, "Failed to unmap h_results");
  }
#endif /* USE_SVM_API == 1 */
  printf("\nVerification: %s\n", pass ? "PASS" : "FAIL");
}

// Free the resources allocated during initialization
void cleanup() {
  for(unsigned i = 0; i < num_devices; ++i) {
    if(kernel && kernel[i]) {
      clReleaseKernel(kernel[i]);
    }
    if(queue && queue[i]) {
      clReleaseCommandQueue(queue[i]);
    }
#if USE_SVM_API == 0
    if(d_indices && d_indices[i]) {
      clReleaseMemObject(d_indices[i]);
    }
    if(d_neighbors && d_neighbors[i]) {
      clReleaseMemObject(d_neighbors[i]);
    }
    if(d_weights && d_weights[i]) {
      clReleaseMemObject(d_weights[i]);
    }
    if(d_worklist1 && d_worklist1[i]) {
      clReleaseMemObject(d_worklist1[i]);
    }
    if(d_worklist2 && d_worklist2[i]) {
      clReleaseMemObject(d_worklist2[i]);
    }
    if(d_results && d_results[i]) {
      clReleaseMemObject(d_results[i]);
    }
#else
    if(h_indices[i].get())
      h_indices[i].reset();
    if(h_neighbors[i].get())
      h_neighbors[i].reset();
    if(h_results[i].get())
      h_results[i].reset();
#endif /* USE_SVM_API == 0 */
  }

  if(program) {
    clReleaseProgram(program);
  }
  if(context) {
    clReleaseContext(context);
  }
}

