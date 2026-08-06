set(ECL_UNICODE_WIDTH "32" CACHE STRING "Unicode character width (bits)")
set(ECL_UNICODE "21" CACHE STRING "Unicode enabled")
set(ECL_CHAR_CODE_LIMIT "" CACHE STRING "Unicode character count")
set(ECL_CHARACTER "" CACHE STRING "Unicode character data type")

if (ECL_ENABLE_UNICODE)
  if(ECL_UNICODE_WIDTH EQUAL "16")
      set(ECL_UNICODE "16")
      set(ECL_CHAR_CODE_LIMIT "65536")
      set(ECL_CHARACTER "${ECL_INT16_T}")

      list(APPEND EXTRA_OBJS unicode/ucd16.o 
                             unicode/ucd16-0000.o
                             unicode/ucd16-0016.o
                             unicode/ucd16-0032.o
                             unicode/ucd16-0048.o
                             unicode/ucd16-0064.o)

  else(ECL_UNICODE_WIDTH EQUAL "21")
      set(ECL_UNICODE "21")
      set(ECL_CHAR_CODE_LIMIT "1114112")
      set(ECL_CHARACTER "${ECL_INT32_T}")
      
      list(APPEND EXTRA_OBJS unicode/ucd.o
                             unicode/ucd-0000.o
                             unicode/ucd-0016.o
                             unicode/ucd-0032.o
                             unicode/ucd-0048.o
                             unicode/ucd-0064.o
                             unicode/ucd-0080.o
                             unicode/ucd-0096.o
                             unicode/ucd-0112.o
                             unicode/ucd-0128.o
                             unicode/ucd-0144.o)

  endif()
  set(ECL_UNICODE_NAMES 1)
  list(APPEND EXTRA_OBJS unicode/ucd_names_char.o
                         unicode/ucd_names_codes.o
                         unicode/ucd_names_pair.o
                         unicode/ucd_names_str.o)

  include(CheckIncludeFiles)
  check_include_files(wchar.h HAVE_WCHAR_H)

  list(APPEND LSP_FEATURES :unicode)

#  foreach(v ${EXTRA_OBJS})
#    message(${v})
#  endforeach()
  
else()
   set(ECL_CHAR_CODE_LIMIT "256")
   set(ECL_CHARACTER "int")
   unset(ECL_UNICODE)

endif()