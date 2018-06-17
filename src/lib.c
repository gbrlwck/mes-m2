/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 *
 * This file is part of Mes.
 *
 * Mes is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or (at
 * your option) any later version.
 *
 * Mes is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Mes.  If not, see <http://www.gnu.org/licenses/>.
 */

int g_depth;
SCM fdisplay_ (SCM, int, int);

SCM
display_helper (SCM x, int cont, char* sep, int fd, int write_p)
{
  fdputs (sep, fd);
  if (g_depth == 0)
    return cell_unspecified;
  g_depth = g_depth - 1;

  switch (TYPE (x))
    {
    case TCHAR:
      {
        if (!write_p)
          fdputc (VALUE (x), fd);
        else
          {
            fdputs ("#\\", fd);
            switch (VALUE (x))
              {
              case '\0': fdputs ("nul", fd); break;
              case '\a': fdputs ("alarm", fd); break;
              case '\b': fdputs ("backspace", fd); break;
              case '\t': fdputs ("tab", fd); break;
              case '\n': fdputs ("newline", fd); break;
              case '\v': fdputs ("vtab", fd); break;
              case '\f': fdputs ("page", fd); break;
                //Nyacc bug
                // case '\r': fdputs ("return", fd); break;
              case 13: fdputs ("return", fd); break;
              case ' ': fdputs ("space", fd); break;
              default: fdputc (VALUE (x), fd);
              }
          }
        break;
      }
    case TCLOSURE:
      {
        fdputs ("#<closure ", fd);
        display_helper (CDR (x), cont, "", fd, 0);
        fdputs (">", fd);
        break;
      }
    case TFUNCTION:
      {
        fdputs ("#<procedure ", fd);
        char const *p = "?";
        if (FUNCTION (x).name != 0)
          p = FUNCTION (x).name;
        fdputs (p, fd);
        fdputs ("[", fd);
        fdputs (itoa (CDR (x)), fd);
        fdputs (",", fd);
        fdputs (itoa (x), fd);
        fdputs ("]>", fd);
        break;
      }
    case TMACRO:
      {
        fdputs ("#<macro ", fd);
        display_helper (CDR (x), cont, "", fd, 0);
        fdputs (">", fd);
        break;
      }
    case TVARIABLE:
      {
        fdputs ("#<variable ", fd);
        display_helper (CAR (VARIABLE (x)), cont, "", fd, 0);
        fdputs (">", fd);
        break;
      }
    case TNUMBER:
      {
        fdputs (itoa (VALUE (x)), fd);
        break;
      }
    case TPAIR:
      {
        if (!cont)
          fdputs ("(", fd);
        if (CAR (x) == cell_circular
            && CADR (x) != cell_closure)
          {
            fdputs ("(*circ* . ", fd);
            int i = 0;
            x = CDR (x);
            while (x != cell_nil && i++ < 10)
              {
                fdisplay_ (CAAR (x), fd, write_p); fdputs (" ", fd);
                x = CDR (x);
              }
            fdputs (" ...)", fd);
          }
        else
          {
            if (x && x != cell_nil)
              fdisplay_ (CAR (x), fd, write_p);
            if (CDR (x) && TYPE (CDR (x)) == TPAIR)
              display_helper (CDR (x), 1, " ", fd, write_p);
            else if (CDR (x) && CDR (x) != cell_nil)
              {
                if (TYPE (CDR (x)) != TPAIR)
                  fdputs (" . ", fd);
                fdisplay_ (CDR (x), fd, write_p);
              }
          }
        if (!cont)
          fdputs (")", fd);
        break;
      }
    case TKEYWORD:
    case TPORT:
    case TSPECIAL:
    case TSTRING:
    case TSYMBOL:
      {
        if (TYPE (x) == TPORT)
          {
            fdputs ("#<port ", fd);
            fdputs (itoa (PORT (x)), fd);
            fdputs (" " ,fd);
          }
        if (TYPE (x) == TKEYWORD)
          fdputs ("#:", fd);
        if ((write_p && TYPE (x) == TSTRING) || TYPE (x) == TPORT)
          fdputc ('"', fd);
        SCM t = CAR (x);
        while (t && t != cell_nil)
          {
            switch (write_p ? VALUE (CAR (t)) : -1)
              {
              case '\0': fdputs ("\\0", fd); break;
              case '\a': fdputs ("\\a", fd); break;
              case '\b': fdputs ("\\b", fd); break;
              case '\t': fdputs ("\\t", fd); break;
              case '\v': fdputs ("\\v", fd); break;
              case '\n': fdputs ("\\n", fd); break;
              case '\f': fdputs ("\\f", fd); break;
#if 1 //__MESC__
      //Nyacc bug
              case 13: fdputs ("\\r", fd); break;
              case 27: fdputs ("\\e", fd); break;
#else
                //case '\r': fdputs ("\\r", fd); break;
                //Nyacc crash
                //case '\e': fdputs ("\\e", fd); break;
#endif
              case '\\': fdputs ("\\\\", fd); break;
              case '"': fdputs ("\\\"", fd); break;
              default:
                fdputc (VALUE (CAR (t)), fd);
              }
            t = CDR (t);
          }
        if ((write_p && TYPE (x) == TSTRING) || TYPE (x) == TPORT)
          fdputc ('"', fd);
        if (TYPE (x) == TPORT)
          fdputs (">", fd);
        break;
      }
    case TVECTOR:
      {
        fdputs ("#(", fd);
        SCM t = CAR (x);
        for (int i = 0; i < LENGTH (x); i++)
          {
            if (i)
              fdputc (' ', fd);
            fdisplay_ (VECTOR (x) + i, fd, write_p);
          }
        fdputc (')', fd);
        break;
      }
    default:
      {
        fdputs ("<", fd);
        fdputs (itoa (TYPE (x)), fd);
        fdputs (":", fd);
        fdputs (itoa (x), fd);
        fdputs (">", fd);
        break;
      }
    }
  return 0;
}

SCM
display_ (SCM x)
{
  g_depth = 5;
  return display_helper (x, 0, "", g_stdout, 0);
}

SCM
display_error_ (SCM x)
{
  g_depth = 5;
  return display_helper (x, 0, "", STDERR, 0);
}

SCM
display_port_ (SCM x, SCM p)
{
  assert (TYPE (p) == TNUMBER);
  return fdisplay_ (x, VALUE (p), 0);
}

SCM
write_ (SCM x)
{
  g_depth = 5;
  return display_helper (x, 0, "", g_stdout, 1);
}

SCM
write_error_ (SCM x)
{
  g_depth = 5;
  return display_helper (x, 0, "", STDERR, 1);
}

SCM
write_port_ (SCM x, SCM p)
{
  assert (TYPE (p) == TNUMBER);
  return fdisplay_ (x, VALUE (p), 1);
}

SCM
fdisplay_ (SCM x, int fd, int write_p) ///((internal))
{
  g_depth = 5;
  return display_helper (x, 0, "", fd, write_p);
}

SCM
exit_ (SCM x) ///((name . "exit"))
{
  assert (TYPE (x) == TNUMBER);
  exit (VALUE (x));
}

SCM
xassq (SCM x, SCM a) ///for speed in core only
{
  while (a != cell_nil && x != CDAR (a))
    a = CDR (a);
  return a != cell_nil ? CAR (a) : cell_f;
}

SCM
memq (SCM x, SCM a)
{
  switch (TYPE (x))
    {
    case TCHAR:
    case TNUMBER:
      {
        SCM v = VALUE (x);
        while (a != cell_nil && v != VALUE (CAR (a)))
          a = CDR (a);
        break;
      }
    case TKEYWORD:
      {
        SCM v = STRING (x);
        while (a != cell_nil && v != STRING (CAR (a)))
          a = CDR (a);
        break;
      }
      // case TSYMBOL:
      // case TSPECIAL:
    default:
      while (a != cell_nil && x != CAR (a))
        a = CDR (a);
    }
  return a != cell_nil ? a : cell_f;
}

SCM
equal2_p (SCM a, SCM b)
{
 equal2:
  if (a == b)
    return cell_t;
  if (TYPE (a) == TPAIR && TYPE (b) == TPAIR)
    {
      if (equal2_p (CAR (a), CAR (b)) == cell_t)
        {
          a = CDR (a);
          b = CDR (b);
          goto equal2;
        }
      return cell_f;
    }
  if (TYPE (a) == TSTRING && TYPE (b) == TSTRING)
    {
      a = STRING (a);
      b = STRING (b);
      goto equal2;
    }
  if (TYPE (a) == TVECTOR && TYPE (b) == TVECTOR)
    {
      if (LENGTH (a) != LENGTH (b))
        return cell_f;
      for (int i=0; i < LENGTH (a); i++)
        {
          SCM ai = VECTOR (a) + i;
          SCM bi = VECTOR (b) + i;
          if (TYPE (ai) == TREF)
            ai = REF (ai);
          if (TYPE (bi) == TREF)
            bi = REF (bi);
          if (equal2_p (ai, bi) == cell_f)
            return cell_f;
        }
      return cell_t;
    }
  return eq_p (a, b);
}

SCM
last_pair (SCM x)
{
  while (x != cell_nil && CDR (x) != cell_nil)
    x = CDR (x);
  return x;
}