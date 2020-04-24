/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * mailbox.d -- thread communication queue
 *
 * Copyright (c) 2012 Juan Jose Garcia Ripoll
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <ecl/ecl.h>
#include <ecl/internal.h>

static ECL_INLINE void
FEerror_not_a_mailbox(cl_object mailbox)
{
  FEwrong_type_argument(@'mp::mailbox', mailbox);
}

cl_object
ecl_make_mailbox(cl_object name, cl_fixnum count)
{
  cl_object output = ecl_alloc_object(t_mailbox);
  cl_fixnum mask;
  for (mask = 1; mask < count; mask <<= 1) {}
  if (mask == 1)
    mask = 63;
  count = mask;
  mask = count - 1;
  output->mailbox.name = name;
  output->mailbox.data = si_make_vector(ECL_T, /* element type */
                                        ecl_make_fixnum(count), /* size */
                                        ECL_NIL, /* adjustable */
                                        ECL_NIL, /* fill pointer */
                                        ECL_NIL, /* displaced to */
                                        ECL_NIL); /* displacement */
  output->mailbox.reader_semaphore =
    ecl_make_semaphore(name, 0);
  output->mailbox.writer_semaphore =
    ecl_make_semaphore(name, count);
  output->mailbox.read_pointer = 0;
  output->mailbox.write_pointer = 0;
  output->mailbox.mask = mask;
  return output;
}

@(defun mp::make-mailbox (&key name (count ecl_make_fixnum(128)))
  @ {
    @(return ecl_make_mailbox(name, fixnnint(count)));
  } @)

cl_object
mp_mailbox_name(cl_object mailbox)
{
  cl_env_ptr env = ecl_process_env();
  unlikely_if (ecl_t_of(mailbox) != t_mailbox) {
    FEerror_not_a_mailbox(mailbox);
  }
  ecl_return1(env, mailbox->mailbox.name);
}

cl_object
mp_mailbox_count(cl_object mailbox)
{
  cl_env_ptr env = ecl_process_env();
  unlikely_if (ecl_t_of(mailbox) != t_mailbox) {
    FEerror_not_a_mailbox(mailbox);
  }
  ecl_return1(env, ecl_make_fixnum(mailbox->mailbox.data->vector.dim));
}

cl_object
mp_mailbox_empty_p(cl_object mailbox)
{
  cl_env_ptr env = ecl_process_env();
  unlikely_if (ecl_t_of(mailbox) != t_mailbox) {
    FEerror_not_a_mailbox(mailbox);
  }
  ecl_return1(env, mailbox->mailbox.reader_semaphore->semaphore.counter? ECL_NIL : ECL_T);
}

cl_object
mp_mailbox_read(cl_object mailbox)
{
  cl_env_ptr env = ecl_process_env();
  cl_fixnum ndx;
  cl_object output;
  unlikely_if (ecl_t_of(mailbox) != t_mailbox) {
    FEerror_not_a_mailbox(mailbox);
  }
  mp_wait_on_semaphore(mailbox->mailbox.reader_semaphore);
  {
    ndx = AO_fetch_and_add1((AO_t*)&mailbox->mailbox.read_pointer) &
      mailbox->mailbox.mask;
    output = mailbox->mailbox.data->vector.self.t[ndx];
  }
  mp_signal_semaphore(1, mailbox->mailbox.writer_semaphore);
  ecl_return1(env, output);
}

cl_object
mp_mailbox_try_read(cl_object mailbox)
{
  cl_env_ptr env = ecl_process_env();
  cl_fixnum ndx;
  cl_object output;
  unlikely_if (ecl_t_of(mailbox) != t_mailbox) {
    FEerror_not_a_mailbox(mailbox);
  }
  output = mp_try_get_semaphore(mailbox->mailbox.reader_semaphore);
  if (output != ECL_NIL) {
    ndx = AO_fetch_and_add1((AO_t*)&mailbox->mailbox.read_pointer) &
      mailbox->mailbox.mask;
    output = mailbox->mailbox.data->vector.self.t[ndx];
    mp_signal_semaphore(1, mailbox->mailbox.writer_semaphore);
  }
  ecl_return1(env, output);
}

cl_object
mp_mailbox_send(cl_object mailbox, cl_object msg)
{
  cl_env_ptr env = ecl_process_env();
  cl_fixnum ndx;
  unlikely_if (ecl_t_of(mailbox) != t_mailbox) {
    FEerror_not_a_mailbox(mailbox);
  }
  mp_wait_on_semaphore(mailbox->mailbox.writer_semaphore);
  {
    ndx = AO_fetch_and_add1((AO_t*)&mailbox->mailbox.write_pointer) &
      mailbox->mailbox.mask;
    mailbox->mailbox.data->vector.self.t[ndx] = msg;
  }
  mp_signal_semaphore(1, mailbox->mailbox.reader_semaphore);
  ecl_return0(env);
}

cl_object
mp_mailbox_try_send(cl_object mailbox, cl_object msg)
{
  cl_env_ptr env = ecl_process_env();
  cl_object output;
  cl_fixnum ndx;
  unlikely_if (ecl_t_of(mailbox) != t_mailbox) {
    FEerror_not_a_mailbox(mailbox);
  }
  output = mp_try_get_semaphore(mailbox->mailbox.writer_semaphore);
  if (output != ECL_NIL) {
    output = msg;
    ndx = AO_fetch_and_add1((AO_t*)&mailbox->mailbox.write_pointer) &
      mailbox->mailbox.mask;
    mailbox->mailbox.data->vector.self.t[ndx] = msg;
    mp_signal_semaphore(1, mailbox->mailbox.reader_semaphore);
  }
  ecl_return1(env, output);
}

