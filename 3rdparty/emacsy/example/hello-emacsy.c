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
 * Let's exercise these functions in a minimal FreeGLUT program we'll call
 * @verb{|hello-emacsy|}.@footnote{Note: Emacsy does not rely on FreeGLUT.
 * One could use Gtk+, Ncurses, Qt, or whatever}.  This simple program
 * will display an integer, the variable @var{counter}, that one can
 * increment or decrement.
 *
 * @image{images/minimal-emacsy-example,,,,.png}
 */

#ifndef SCM_MAGIC_SNARFER
#include <libgen.h>
#ifdef __APPLE__
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif
#include <emacsy.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#endif
#include <libguile.h>

void display_func ();
void keyboard_func (unsigned char glut_key, int x, int y);
void draw_string (int, int, char*);
char * try_load_startup (char const* prefix, char const* dir, char const* startup_script);
void primitives_init ();

/*
 * @defvar int counter = 0;
 * Hello Emacsy's state is captured by one global variable.
 * Hello Emacsy will display this number.
 * @end defvar
 */
int counter = 0;
int interactive = 1;

/*
 * Initialize everything in @var{main} and enter our runloop.
 */
int
main (int argc, char *argv[])
{
  int err;
/* glutInit (&argc, argv);
 * Initialize GLUT.
 */
  glutInit (&argc, argv);
  glutInitDisplayMode (GLUT_RGB|GLUT_DOUBLE);
  glutInitWindowSize (500, 500);
  glutCreateWindow ("Hello, Emacsy!");
  glutDisplayFunc (display_func);
  if (interactive)
    glutKeyboardFunc (keyboard_func);
/* void scm_init_guile ();
 * Initialize Guile.
 */
  scm_init_guile ();
/* emacsy_initialize (@dots {});
 * Initialize Emacsy.
 */
  if (argc == 2 && strcmp ("--batch", argv[1]) == 0)
    interactive = 0;
  err = emacsy_initialize (interactive
                           ? EMACSY_INTERACTIVE
                           : EMACSY_NON_INTERACTIVE);
  if (err)
    exit (err);
/* primitives_init ();
 * Register primitives.
 */
  primitives_init ();

/* char * try_load_startup (@dots{});
 * Try to load @file{hello-emacsy.scm}
 */
  char const *startup_script = "hello-emacsy.scm";

  char prefix[PATH_MAX];
  strcpy (prefix, argv[0]);
  if (getenv ("_"))
    strcpy (prefix, getenv ("_"));
  dirname (dirname (prefix));

  if (!try_load_startup (0, 0, startup_script)
      &&!try_load_startup (getenv ("EMACSY_SYSCONFDIR"), "/", startup_script)
      &&!try_load_startup (prefix, "/", startup_script)
      &&!try_load_startup (prefix, "/etc/emacsy/", startup_script))
    fprintf (stderr, "error: failed to find '%s'.\n", startup_script);
/* void glutMainLoop ();
 * Enter GLUT main loop, not return.
 */
  glutMainLoop ();
  return 0;
}

/*
 * @subsection Runloop Interaction
 *
 * Let's look at how Emacsy interacts with your application's runloop
 * since that's probably the most concerning part of embedding.  First,
 * let's pass some input to Emacsy.
 */

/* void keyboard_func (unsigned char glut_key, int x, int y)
 * Send key events to Emacsy.
 */
void
keyboard_func (unsigned char glut_key, int x, int y)
{
/*
 * int key; // The Key event (not processed yet).
 */
  int key;
  int mod_flags;
  int glut_mod_flags = glutGetModifiers ();
  mod_flags = 0;
  if (glut_mod_flags & GLUT_ACTIVE_SHIFT)
    mod_flags |= EMACSY_MODKEY_SHIFT;
  if (glut_mod_flags & GLUT_ACTIVE_CTRL)
    mod_flags |= EMACSY_MODKEY_CONTROL;
  if (glut_mod_flags & GLUT_ACTIVE_ALT)
    mod_flags |= EMACSY_MODKEY_META;
  if (glut_key == 8)
    glut_key = 127;
  else if (glut_key == 127)
    {
      glut_key = 4;
      mod_flags += EMACSY_MODKEY_CONTROL;
    }
/*
 * The keys @verb{|C-a|} and @verb{|C-b|} return @code{1} and @code{2}
 * respectively.  We want to map these to their actual character values.
 */
  key = mod_flags & EMACSY_MODKEY_CONTROL
    ? glut_key + ('a' - 1)
    : glut_key;
  emacsy_key_event (key, mod_flags);
  glutPostRedisplay ();
}

/* void display_func ()
 * The function @var{display_func} is run for every frame that's
 * drawn. It's effectively our runloop, even though the actual runloop is
 * in FreeGLUT.
 *
 * Our application has just one job: Display the counter variable.
 */
void
display_func ()
{
/* glClear (GL_COLOR_BUFFER_BIT);
 * Setup the display buffer the drawing.
 */
  glClear (GL_COLOR_BUFFER_BIT);

  glMatrixMode (GL_PROJECTION);
  glLoadIdentity ();
  glOrtho (0.0, 500.0, 0.0, 500.0, -2.0, 500.0);
  gluLookAt (0,   0,   2,
             0.0, 0.0, 0.0,
             0.0, 1.0, 0.0);

  glMatrixMode (GL_MODELVIEW);
  glColor3f (1, 1, 1);

  char counter_string[255];
  sprintf (counter_string, "%d", counter);
  draw_string (250, 250, counter_string);

/*
 * Process events in Emacsy.
 */
  if (emacsy_tick () & EMACSY_QUIT_APPLICATION_P)
    {
      emacsy_terminate ();
      exit (0);
    }
  glutSetWindowTitle (emacsy_current_buffer ());

/*
 * Display Emacsy message/echo area.
 */
  draw_string (0, 5, emacsy_message_or_echo_area ());

/*
 *  Display Emacsy mode line.
 */
  draw_string (0, 30, emacsy_mode_line ());

  glutSwapBuffers ();
}

/* void draw_string (int x, int y, char *string)
 *
 * Draw a string function.
 * Draws a string at (x, y) on the screen.
 */
void
draw_string (int x, int y, char *string)
{
  glLoadIdentity ();
  glTranslatef (x, y, 0.);
  glScalef (0.2, 0.2, 1.0);
  while (*string)
    glutStrokeCharacter (GLUT_STROKE_ROMAN,
                         *string++);
}

/*
 * At this point, our application can process key events, accept input on
 * the minibuffer, and use nearly all of the facilities that Emacsy
 * offers, but it can't change any application state, which makes it not
 * very interesting yet.
 */

/*
 * @subsection Plugging Into Your App
 */

//

/*
 * @deffn {Scheme Procedure} get-counter
 * @deffnx {C Function} SCM scm_get_counter ()
 * Let's define a new primitive Scheme procedure @var{get-counter}, so
 * Emacsy can access the application's state.  This will define
 * a @var{C} function @code{SCM scm_get_counter (void)} and a Scheme procedure
 * @code{(get-counter)}.
 *
 * @end deffn
 */

SCM_DEFINE (scm_get_counter, "get-counter",
            /* required arg count    */ 0,
            /* optional arg count    */ 0,
            /* variable length args? */ 0,
            (),
            "Returns value of counter.")
{
  return scm_from_int (counter);
}

/*
 * @deffn {Scheme Procedure} set-counter! value
 * @deffnx {C Function} SCM scm_set_counter_x (SCM value)
 * Let's define another primitive Scheme procedure to alter the
 * application's state.
 * @end deffn
 */

SCM_DEFINE (scm_set_counter_x, "set-counter!",
            /* required, optional, var. length? */
            1, 0, 0,
            (SCM value),
            "Sets value of counter.")
{
  counter = scm_to_int (value);
  glutPostRedisplay ();
  return SCM_UNSPECIFIED;
}

/* void primitives_init ()
 * Once we have written these primitive procedures, we need to register
 * them with the Scheme runtime.
 */
void
primitives_init ()
{
#ifndef SCM_MAGIC_SNARFER
#include "hello-emacsy.c.x"
#endif
}

/* char * try_load_startup (char const* prefix, char const* dir, char const* startup_script)
 * Locate the @file{hello-emacsy.scm} Guile initialization and load it.
 */
char *
try_load_startup (char const* prefix, char const* dir, char const* startup_script)
{
  static char file_name[PATH_MAX];
  if (prefix)
    strcpy (file_name, prefix);
  if (dir)
    strcat (file_name, dir);
  strcat (file_name, startup_script);

  if (access (file_name, R_OK) != -1)
    {
      fprintf (stderr, "Loading '%s'.\n", file_name);
      scm_c_primitive_load (file_name);
      return file_name;
    }
  else
    fprintf (stderr, "no such file '%s'.\n", file_name);

  return 0;
}
