FRAMAC_SHARE  :=$(shell frama-c.byte -print-path)
FRAMAC_LIBDIR :=$(shell frama-c.byte -print-libpath)
PLUGIN_NAME = DumpCIL
PLUGIN_CMO  = dump_cil

include $(FRAMAC_SHARE)/Makefile.dynamic

.PHONY:clean-all
clean-all: clean
	-rm -f .depend
	-rm -f $(FRAMAC_LIBDIR)/DumpCIL.*
