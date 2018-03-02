/* -*- Mode: C; c-basic-offset: 8; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=8 shiftwidth=4 expandtab: */

/*
    ecl-atomic-ops.h -- Wrapper around libatomic_ops functions
*/
/*
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#ifndef ECL_ATOMIC_OPS_H
#define ECL_ATOMIC_OPS_H

#ifdef ECL_THREADS
# define AO_REQUIRE_CAS
# ifdef ECL_LIBATOMIC_OPS_H
#  include <ecl/atomic_ops.h>
# else
#  include <atomic_ops.h>
# endif
#else
# define AO_load(x) (x)
# define AO_store(x,y) ((x)=(y))
# define AO_nop_full()
#endif

#endif /* ECL_ATOMIC_OPS_H */
