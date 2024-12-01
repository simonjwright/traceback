all:
	echo "be specific!"

raw: Makefile
	rm -rf $@
	mkdir $@
	(cd $@;								\
	 g++								\
	   -c								\
	   -fdump-ada-spec-slim						\
	   $(shell xcrun --show-sdk-path)/usr/include/mach-o/loader.h;	\
	 g++								\
	   -c								\
	   -fdump-ada-spec-slim						\
	   $(shell xcrun --show-sdk-path)/usr/include/mach-o/nlist.h)

# These conversions (a) remove the package names derived from the
# Darwin headers, (b) use the type names defined in s-objrea.ads
# (System.Object_Reader).
#
# There are still some Interfaces.C & Interfaces.C.Extended types.
src: raw Makefile
	rm -rf $@
	mkdir -p $@
	(cd $<	;						\
	  for f in *; do					\
	   sed							\
	     -e "s/^with utypes/--  &/g"			\
	     -e "s/^with sys/--  &/g"				\
	     -e "s/utypes_uuint32_t_h.uint32_t/uint32/g"	\
	     -e "s/utypes_uuint64_t_h.uint64_t/uint64/g"	\
	     -e "s/sys_utypes_uint32_t_h.int32_t/int32/g"	\
	     -e "s/sys_utypes_uint32_t_h/uint32/g"		\
	     -e "s/utypes_uuint8_t_h.uint8_t/uint8/g"		\
	     -e "s/utypes_uuint16_t_h.uint16_t/uint16/g"	\
	     $$f > ../$@/$$f;					\
	done)

clean:
	rm -rf raw
.PHONY: clean
