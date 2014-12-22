# Copyright (c) 2012-2015, Bluespec, Inc., All Rights Reserved
# Author: Rishiyur S. Nikhil

# ----------------------------------------------------------------
# These routines are used to communicate between C and BSV

	.file	"cpAsm.s"
	.ctext
# Change the following line for your custom personality signature
	.signature	pdk=65125

# ----------------------------------------------------------------
	.globl	cpWrAEG0
	.type	cpWrAEG0, @function

cpWrAEG0:
	mov %a8,  $0, %aeg
	rtn

# ----------------------------------------------------------------
	.globl	cpRdAEG0
	.type	cpRdAEG0, @function

cpRdAEG0:
	mov.ae0 %aeg, $0, %a8
	rtn

# ----------------------------------------------------------------
	.globl	cpCaep
	.type	cpCaep, @function

cpCaep:
	mov %a8,  $0, %aeg
	caep00 $0
	rtn

# ----------------------------------------------------------------

	.cend
