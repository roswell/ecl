;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya
;;;;  Copyright (c) 1990, Giuseppe Attardi
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;  Copyright (c) 2021, Daniel Kochmański
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;

(in-package #:compiler)

(defun c2with-stack (c1form body)
  (declare (ignore c1form))
  (let* ((new-destination (tmp-destination *destination*))
         (*temp* *temp*))
    (wt-nl-open-brace)
    (wt-nl "struct ecl_stack_frame _ecl_inner_frame_aux;")
    (wt-nl *volatile* "cl_object _ecl_inner_frame = ecl_stack_frame_open(cl_env_copy,(cl_object)&_ecl_inner_frame_aux,0);")
    (let* ((*destination* new-destination)
           (*unwind-exit* `((STACK "_ecl_inner_frame") ,@*unwind-exit*)))
      (c2expr* body))
    (wt-nl "ecl_stack_frame_close(_ecl_inner_frame);")
    (wt-nl-close-brace)
    (unwind-exit new-destination)))

(defun c2stack-push-values (c1form form push-statement)
  (declare (ignore c1form))
  (let ((*destination* 'VALUES))
    (c2expr* form))
  (c2expr push-statement))
