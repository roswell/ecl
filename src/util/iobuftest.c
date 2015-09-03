/* -*- Mode: C; c-basic-offset: 8; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=8 shiftwidth=4 expandtab: */

#include <stdio.h>
#include "../machine.h"

/*
 * On some machines (Solaris 2), the FILE structure stores the
 * buffer passed by setbuf() with an offset
 */

char buf[BUFSIZ];

main()
{
  setbuf(stdin, buf);
  printf("%d\n", (int)buf - (int)stdin->_IO_buf_base);
}
