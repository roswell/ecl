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

/* NOTE: The mailbox functions are not interrupt safe. */

cl_object
ecl_make_mailbox(cl_object name, cl_fixnum count)
{
  cl_env_ptr env = ecl_process_env();
  cl_object output = ecl_alloc_object(t_mailbox);
  output->mailbox.name = name;
  output->mailbox.data = si_make_vector(ECL_T, /* element type */
                                        ecl_make_fixnum(count), /* size */
                                        ECL_NIL, /* adjustable */
                                        ECL_NIL, /* fill pointer */
                                        ECL_NIL, /* displaced to */
                                        ECL_NIL); /* displacement */
  output->mailbox.message_count = 0;
  output->mailbox.read_pointer = 0;
  output->mailbox.write_pointer = 0;
  ecl_disable_interrupts_env(env);
  ecl_mutex_init(&output->mailbox.mutex, FALSE);
  ecl_cond_var_init(&output->mailbox.reader_cv);
  ecl_cond_var_init(&output->mailbox.writer_cv);
  ecl_set_finalizer_unprotected(output, ECL_T);
  ecl_enable_interrupts_env(env);
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
    FEwrong_type_only_arg(@[mp::mailbox-name], mailbox, @[mp::mailbox]);
  }
  ecl_return1(env, mailbox->mailbox.name);
}

cl_object
mp_mailbox_count(cl_object mailbox)
{
  cl_env_ptr env = ecl_process_env();
  unlikely_if (ecl_t_of(mailbox) != t_mailbox) {
    FEwrong_type_only_arg(@[mp::mailbox-count], mailbox, @[mp::mailbox]);
  }
  ecl_return1(env, ecl_make_fixnum(mailbox->mailbox.data->vector.dim));
}

cl_object
mp_mailbox_empty_p(cl_object mailbox)
{
  cl_env_ptr env = ecl_process_env();
  unlikely_if (ecl_t_of(mailbox) != t_mailbox) {
    FEwrong_type_only_arg(@[mp::mailbox-empty-p], mailbox, @[mp::mailbox]);
  }
  ecl_return1(env, mailbox->mailbox.message_count? ECL_NIL : ECL_T);
}

static cl_object
read_message(cl_object mailbox)
{
  cl_object output;
  cl_fixnum ndx = mailbox->mailbox.read_pointer++;
  if (mailbox->mailbox.read_pointer >= mailbox->mailbox.data->vector.dim) {
    mailbox->mailbox.read_pointer = 0;
  }
  output = mailbox->mailbox.data->vector.self.t[ndx];
  mailbox->mailbox.message_count--;
  ecl_cond_var_signal(&mailbox->mailbox.writer_cv);
  return output;
}

cl_object
mp_mailbox_read(cl_object mailbox)
{
  cl_env_ptr env = ecl_process_env();
  cl_object output;
  unlikely_if (ecl_t_of(mailbox) != t_mailbox) {
    FEwrong_type_only_arg(@[mp::mailbox-read], mailbox, @[mp::mailbox]);
  }
  ecl_mutex_lock(&mailbox->mailbox.mutex);
  {
    while (mailbox->mailbox.message_count == 0) {
      ecl_cond_var_wait(&mailbox->mailbox.reader_cv, &mailbox->mailbox.mutex);
    }
    output = read_message(mailbox);
  }
  ecl_mutex_unlock(&mailbox->mailbox.mutex);
  ecl_return1(env, output);
}

cl_object
mp_mailbox_try_read(cl_object mailbox)
{
  cl_env_ptr env = ecl_process_env();
  cl_object output;
  unlikely_if (ecl_t_of(mailbox) != t_mailbox) {
    FEwrong_type_only_arg(@[mp::mailbox-try-read], mailbox, @[mp::mailbox]);
  }
  ecl_mutex_lock(&mailbox->mailbox.mutex);
  {
    if (mailbox->mailbox.message_count == 0) {
      output = ECL_NIL;
    } else {
      output = read_message(mailbox);
    }
  }
  ecl_mutex_unlock(&mailbox->mailbox.mutex);
  ecl_return1(env, output);
}

static void
store_message(cl_object mailbox, cl_object msg)
{
  cl_fixnum ndx = mailbox->mailbox.write_pointer++;
  if (mailbox->mailbox.write_pointer >= mailbox->mailbox.data->vector.dim) {
    mailbox->mailbox.write_pointer = 0;
  }
  mailbox->mailbox.data->vector.self.t[ndx] = msg;
  mailbox->mailbox.message_count++;
  ecl_cond_var_signal(&mailbox->mailbox.reader_cv);
}

cl_object
mp_mailbox_send(cl_object mailbox, cl_object msg)
{
  cl_env_ptr env = ecl_process_env();
  unlikely_if (ecl_t_of(mailbox) != t_mailbox) {
    FEwrong_type_only_arg(@[mp::mailbox-send], mailbox, @[mp::mailbox]);
  }
  ecl_mutex_lock(&mailbox->mailbox.mutex);
  {
    while (mailbox->mailbox.message_count == mailbox->mailbox.data->vector.dim) {
      ecl_cond_var_wait(&mailbox->mailbox.writer_cv, &mailbox->mailbox.mutex);
    }
    store_message(mailbox, msg);
  }
  ecl_mutex_unlock(&mailbox->mailbox.mutex);
  ecl_return1(env, msg);
}

cl_object
mp_mailbox_try_send(cl_object mailbox, cl_object msg)
{
  cl_env_ptr env = ecl_process_env();
  cl_object output;
  unlikely_if (ecl_t_of(mailbox) != t_mailbox) {
    FEwrong_type_only_arg(@[mp::mailbox-try-send], mailbox, @[mp::mailbox]);
  }
  ecl_mutex_lock(&mailbox->mailbox.mutex);
  {
    if (mailbox->mailbox.message_count == mailbox->mailbox.data->vector.dim) {
      output = ECL_NIL;
    } else {
      store_message(mailbox, msg);
      output = msg;
    }
  }
  ecl_mutex_unlock(&mailbox->mailbox.mutex);
  ecl_return1(env, output);
}

