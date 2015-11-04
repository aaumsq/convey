#ifndef __PCM_H__
#define __PCM_H__


#include <cpucounters.h>

struct PCMEvent {
    const char * name;
    unsigned char event;
    unsigned char umask;
    const char * description;
};

PCM* m;

void initPCM(PCMEvent* WSMEvents) {
    m = PCM::getInstance();
    
    PCM::ExtendedCustomCoreEventDescription conf;
    conf.fixedCfg = NULL; // default
    conf.nGPCounters = 4;
    EventSelectRegister regs[4];
    conf.gpCounterCfg = regs;
    EventSelectRegister def_event_select_reg;
    def_event_select_reg.value = 0;
    def_event_select_reg.fields.usr = 1;
    def_event_select_reg.fields.os = 1;
    def_event_select_reg.fields.enable = 1;
    for(int i=0;i<4;++i)
        regs[i] = def_event_select_reg;
    
    for(int i = 0; i < 4; i++) {
        regs[i].fields.event_select = WSMEvents[i].event;
        regs[i].fields.umask = WSMEvents[i].umask;
    }
    
    PCM::ErrorCode status = m->program(PCM::EXT_CUSTOM_CORE_EVENTS, &conf);
    switch(status) {
    case PCM::Success:
        break;
    case PCM::MSRAccessDenied:
        std::cerr << "Access to Intel(r) Performance Counter Monitor has denied (no MSR or PCI CFG space access).\n";
        exit(EXIT_FAILURE);
    case PCM::PMUBusy:
        std::cerr << "Access to Intel(r) Performance Counter Monitor has denied (Performance Monitoring Unit is occupied by other application). Try to stop the application that uses PMU.\n";
        std::cerr << "Alternatively you can try to reset PMU configuration at your own risk. Try to reset? (y/n)\n";
        char yn;
        std::cin >> yn;
        if ('y' == yn) {
            m->resetPMU();
            std::cerr << "PMU configuration has been reset. Try to rerun the program again.\n";
        }
        exit(EXIT_FAILURE);
    default:
        std::cerr << "Access to Intel(r) Performance Counter Monitor has denied (Unknown error).\n";
        exit(EXIT_FAILURE);
    }
    
    std::cerr << "\nDetected "<< m->getCPUBrandString() << " \"Intel(r) microarchitecture codename "<<m->getUArchCodename()<<"\"\n";
    
}

void printStats(PCMEvent* WSMEvents, SystemCounterState before_sstate, SystemCounterState after_sstate) {
    uint64_t insts = getInstructionsRetired(before_sstate, after_sstate);
    uint64_t cycles = getCycles(before_sstate, after_sstate);
    
    for(int i = 0; i < 4; i++) {
        uint64_t counter = getNumberOfCustomEvents(i, before_sstate, after_sstate);
        std::cout << WSMEvents[i].name << ": " << counter 
                  << ", PKI: " << double(counter)*1000.0/double(insts) 
                  << ", PKC: " << double(counter)*1000.0/double(cycles) << "\n";
    }

    
    std::cout << "Instructions retired: " << insts << "\n";
    std::cout << "Cycles: " << cycles << "\n";
    std::cout << "IPC: " << getIPC(before_sstate, after_sstate) << "\n";
    std::cout << "Calc IPC: " << double(insts)/double(cycles) << "\n";
    
    std::cout << "Instructions per clock: " << getIPC(before_sstate,after_sstate)
              << "\nL2 cache hit ratio: " << getL2CacheHitRatio(before_sstate,after_sstate)
              << "\nL3 cache hit ratio: " << getL3CacheHitRatio(before_sstate,after_sstate)
              << "\nL2 cache hits: " << getL2CacheHits(before_sstate,after_sstate)
              << "\nL3 cache hits: " << getL3CacheHits(before_sstate,after_sstate)
              << "\nWasted cycles caused by L2 misses: " << getCyclesLostDueL2CacheMisses(before_sstate,after_sstate)
	      << "\nWasted cycles caused by L3 misses: " << getCyclesLostDueL3CacheMisses(before_sstate,after_sstate)
              << "\nBytes read from DRAM: " << getBytesReadFromMC(before_sstate,after_sstate)
              << "\n";
}

#endif
