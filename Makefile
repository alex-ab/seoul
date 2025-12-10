#
# This Makefile is supposed to be executed via Genode's GOA SDK
#

VERBOSE ?= @

SHAPE_DIR = genode

INC_DIR += include genode/include

# pointer/shape_report.h
INC_DIR += $(SHAPE_DIR)/repos/os/include

SRC_CC += $(wildcard model/*.cc)
SRC_CC += $(wildcard executor/*.cc)
SRC_CC += $(filter-out genode/xhci.cc, $(wildcard genode/*.cc))
SRC_CC += base/lib/runtime/string.cc

# all objects
OBJS=$(SRC_CC:.cc=.o) binary_mono_tff.o

#
# Prevent the definition of __STDC_HOSTED__ by default
#
CC_OPT_FREESTANDING ?= -ffreestanding

CCFLAGS += $(CC_OPT_FREESTANDING)
CCFLAGS += -Wno-unused
CCFLAGS += -m64 -mcmodel=large -march=x86-64-v2

symbol_name = _binary_$(subst -,_,$(subst .,_,$(subst binary_,,$(subst .o,,$(notdir $@)))))

%.o: %.cc
	@echo "CC $@"
	$(VERBOSE) $(CXX) $(CCFLAGS) $(CXXFLAGS) $(CPPFLAGS) $(INC_DIR:%=-I %) $< -c -o $@

binary_%.o: genode/mono.tff
	@echo "CONVERT $@"
	$(VERBOSE)echo ".global $(symbol_name)_start, $(symbol_name)_end; .data; .align 4; $(symbol_name)_start:; .incbin \"$<\"; $(symbol_name)_end:" |\
		$(AS) $(AS_OPT) -f -o $@ -

seoul: $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $^ $(LDLIBS)
