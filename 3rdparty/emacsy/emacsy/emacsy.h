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

#ifndef __EMACSY_H
#define __EMACSY_H 1

#ifdef __cplusplus
 extern "C" {
#endif

#include <libguile.h>

/* Here are the constants for the C API.                                    */
/*                                                                          */
/*                                                                          */
/* <emacsy-c-api:Defines>=                                                  */
#define EMACSY_MODKEY_COUNT   6

#define EMACSY_MODKEY_ALT     1 // A
#define EMACSY_MODKEY_CONTROL 2 // C
#define EMACSY_MODKEY_HYPER   4 // H
#define EMACSY_MODKEY_META    8 // M
#define EMACSY_MODKEY_SUPER  16 // s
#define EMACSY_MODKEY_SHIFT  32 // S

#define EMACSY_MOUSE_BUTTON_DOWN  0
#define EMACSY_MOUSE_BUTTON_UP    1
#define EMACSY_MOUSE_MOTION       2

#define EMACSY_INTERACTIVE        1
#define EMACSY_NON_INTERACTIVE    0

/* Here are the return flags that may be returned by \verb|emacsy_tick|.    */
/*                                                                          */
/*                                                                          */
/* <emacsy-c-api:Defines>=                                                  */
#define EMACSY_QUIT_APPLICATION_P       1
#define EMACSY_ECHO_AREA_UPDATED_P      2
#define EMACSY_MODELINE_UPDATED_P       4
#define EMACSY_RAN_UNDEFINED_COMMAND_P  8

/*
 * Emacsy provides a C API to ease integration with C and C++
 * programs. The C API is given below.
 */

/* Initialize Emacsy. */
int  emacsy_initialize (int init_flags);

/* Enqueue a keyboard event. */
void emacsy_key_event (int char_code,
                       int modifier_key_flags);

/* Enqueue a mouse event. */
void emacsy_mouse_event (int x, int y,
                         int state,
                         int button,
                         int modifier_key_flags);

/* Run an iteration of Emacsy's event loop, does not block. */
int emacsy_tick ();

/* Return the message or echo area. */
char *emacsy_message_or_echo_area ();

/* Return the mode line. */
char *emacsy_mode_line ();

/* Return the name of the current buffer. */
char *emacsy_current_buffer ();

/* Run a hook. */
int  emacsy_run_hook_0 (char const *hook_name);

/* Return the minibuffer point. */
int  emacsy_minibuffer_point ();

/* Terminate Emacsy; run termination hook. */
int  emacsy_terminate ();

/* Attempt to load a module. */
int emacsy_load_module (char const *module_name);

/* Load a file in the emacsy environment. */
//int emacsy_load(const char *file_name);

/* Convert the modifier_key_flags into a Scheme list of symbols. */
SCM modifier_key_flags_to_list(int modifier_key_flags);

/* SCM scm_c_string_to_symbol (char const* str) */
SCM scm_c_string_to_symbol (char const* str);

/* Ref @var{name} from emacsy module. */
SCM scm_c_emacsy_ref (char const* name);

#ifdef __cplusplus
 }
#endif

#endif // __EMACSY_H
