/*
  Emacsy --- An embeddable Emacs-like library using GNU Guile

  Copyright (C) 2012, 2013 Shane Celis <shane.celis@gmail.com>
  Copyright (C) 2019, Jan (janneke) Nieuwenhuizen <janneke@gnu.org>

  This file is part of Emacsy.

  Emacsy is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Emacsy is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Emacsy.  If not, see <http://www.gnu.org/licenses/>.
*/

/*
  ;;; Commentary:

  ;; @c C Api
  ;; @c @section C Api

  ;;; Code:
*/

/*
 * @node C Api
 * @section C Api
 */

#include "emacsy.h"
#include <libguile.h>
#include <stdio.h>

/*
 * Emacsy provides a C API to ease integration with C and C++
 * programs. The C API is given below.
 */

/* int emacsy_initialize (int init_flags)
 * Initialize Emacsy.
 */
int
emacsy_initialize (int init_flags)
{
  /* Load the (emacsy emacsy) module. */
  const char *module = "emacsy emacsy";
  int err = emacsy_load_module (module);
  if (err)
    return err;

  (void) scm_call_1 (scm_c_emacsy_ref ("emacsy-initialize"),
                     (init_flags & EMACSY_INTERACTIVE) ? SCM_BOOL_T : SCM_BOOL_F);

  return err;
}

/* void emacsy_key_event (int char_code, int modifier_key_flags)
 * Enqueue a keyboard event.
 */
void
emacsy_key_event (int char_code, int modifier_key_flags)
{
  SCM i = scm_from_int (char_code);
  //fprintf (stderr, "i = %d\n", scm_to_int (i));
  SCM c = scm_integer_to_char (i);
  //fprintf (stderr, "c = %d\n", scm_to_int (scm_char_to_integer (c)));

  (void) scm_call_2 (scm_c_emacsy_ref ("emacsy-key-event"),
                     c,
                     modifier_key_flags_to_list (modifier_key_flags));
}

/* void emacsy_mouse_event (int x, int y, int state, int button, int modifier_key_flags)
 * Enqueue a mouse event.
 */
void
emacsy_mouse_event (int x, int y,
                    int state,
                    int button,
                    int modifier_key_flags)
{
  SCM down_sym   = scm_c_string_to_symbol ("down");
  SCM up_sym     = scm_c_string_to_symbol ("up");
  SCM motion_sym = scm_c_string_to_symbol ("motion");
  SCM state_sym;
  switch (state) {
  case EMACSY_MOUSE_BUTTON_UP:   state_sym = up_sym;     break;
  case EMACSY_MOUSE_BUTTON_DOWN: state_sym = down_sym;   break;
  case EMACSY_MOUSE_MOTION:      state_sym = motion_sym; break;
  default:
    fprintf (stderr, "warning: mouse event state received invalid input %d.\n",
             state);
    return;
  }

  (void) scm_call_3 (scm_c_emacsy_ref ("emacsy-mouse-event"),
                     scm_vector (scm_list_2 (scm_from_int (x),
                                             scm_from_int (y))),
                     scm_from_int (button),
                     state_sym);
}

/* int emacsy_tick ()
 * Run an iteration of Emacsy's event loop, does not block.
 */
int
emacsy_tick ()
{
  int flags = 0;
  (void) scm_call_0 (scm_c_emacsy_ref ("emacsy-tick"));
  if (scm_is_true (scm_c_emacsy_ref ("emacsy-quit-application?")))
    flags |= EMACSY_QUIT_APPLICATION_P;
  if (scm_is_true (scm_c_emacsy_ref ("emacsy-ran-undefined-command?")))
    flags |= EMACSY_RAN_UNDEFINED_COMMAND_P;

  return flags;
}

/* char *emacsy_message_or_echo_area ()
 */
char
*emacsy_message_or_echo_area ()
{
  return scm_to_locale_string
    (scm_call_0 (scm_c_emacsy_ref ("emacsy-message-or-echo-area")));
}

/* char *emacsy_mode_line ()
 * Return the mode line.
 */
char *
emacsy_mode_line ()
{
  return scm_to_locale_string
    (scm_call_0 (scm_c_emacsy_ref ("emacsy-mode-line")));
}

/* char *emacsy_current_buffer ()
 */
char *
emacsy_current_buffer ()
{
  return scm_to_locale_string
    (scm_call_1 (scm_c_emacsy_ref ("buffer-name"),
                 scm_call_0 (scm_c_emacsy_ref ("current-buffer"))));
}

/* int emacsy_run_hook_0 (char const *hook_name)
 * Run a hook.
 */
int
emacsy_run_hook_0 (char const *hook_name)
{
  /* This should be protected from all sorts of errors that the hooks
     could throw. */
  SCM result;
  result = scm_call_1 (scm_c_emacsy_ref ("emacsy-run-hook"),
                       scm_c_private_ref ("guile-user", hook_name));
  return 0;
}

/* int emacsy_minibuffer_point ()
 * Return the minibuffer point.
 */
int
emacsy_minibuffer_point ()
{
  return scm_to_int (scm_call_0 (scm_c_emacsy_ref ("emacsy-minibuffer-point")));
}

/* int emacsy_terminate ()
 * Terminate Emacsy; run termination hook.
 */
int
emacsy_terminate ()
{
  SCM result;
  result = scm_call_0 (scm_c_emacsy_ref ("emacsy-terminate"));
  return 0;
}

/* SCM load_module_try (void* data)
 * Attempt to load a module.
 *
 * The function @var{scm_c_use_module} throws an exception if it cannot
 * find the module, so we have to split that functionality into a body
 * function @var{load_module_try} and an error handler
 * @var{load_module_error}.
 */
SCM
load_module_try (void *data)
{
  scm_c_use_module ((char const *) data);
  return scm_list_1 (SCM_BOOL_T);
}

/* SCM load_module_error (void *data, SCM key, SCM args)
 */
SCM
load_module_error (void *data, SCM key, SCM args)
{
  //fprintf (stderr, "error: Unable to load module (%s).\n", (char const*) data);
  return scm_list_3 (SCM_BOOL_F, key, args);
}

/* int emacsy_load_module (char const *module)
 * Attempt to load a module.  Returns 0 if no errors, and non-zero otherwise.
 */
int
emacsy_load_module (char const *module)
{
  SCM result = scm_internal_catch (SCM_BOOL_T,
                                   load_module_try, (void *) module,
                                   load_module_error, (void *) module);
  if (scm_is_false (scm_car (result))) {
    fprintf (stderr, "error: Unable to load module (%s); got error to key %s with args %s. Try setting the "
             "GUILE_LOAD_PATH environment variable.\n", module,
             scm_to_locale_string (scm_car (scm_cdr (result))),
             scm_to_locale_string (scm_car (scm_cdr (scm_cdr (result))))
             );
    return 1; //EMACSY_ERR_NO_MODULE;
  }
  return 0;
}

/* SCM modifier_key_flags_to_list (int modifier_key_flags)
 */
SCM
modifier_key_flags_to_list (int modifier_key_flags)
{
  const char* modifiers[] = { "alt", "control", "hyper", "meta", "super", "shift" };
  SCM list = SCM_EOL;
  for (int i = 0; i < EMACSY_MODKEY_COUNT; i++)
    if (modifier_key_flags & (1 << i))
      list = scm_cons (scm_c_string_to_symbol (modifiers[i]), list);

  return list;
}

/* SCM scm_c_string_to_symbol (char const* str)
 */
SCM
scm_c_string_to_symbol (char const* str)
{
  return scm_string_to_symbol (scm_from_locale_string (str));
}

/*
 * @deffn {Scheme Procedure} modifier-key-flags->list flags
 * @deffnx {C Function} SCM scm_modifier_key_flags_to_list (flags)
 * Convert integer @var{flags} to a list of symbols.
 * @end deffn
 */

SCM_DEFINE (scm_modifier_key_flags_to_list, "modifier-key-flags->list",
            1, 0, 0,
            (SCM flags),
            "Convert an integer of modifier key flags to a list of symbols.")
{
  int modifier_key_flags = scm_to_int (flags);
  return modifier_key_flags_to_list (modifier_key_flags);
}

/* SCM scm_c_emacsy_ref (char const* name)
 * Ref @var{name} from emacsy module.
 */
SCM
scm_c_emacsy_ref (char const* name)
{
  return scm_c_public_ref ("emacsy emacsy", name);
}
