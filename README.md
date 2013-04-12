        )          *    (    (       )       (       )
     ( /(  *   ) (  `   )\ ) )\ ) ( /(       )\ ) ( /(
     )\())` )  /( )\))( (()/((()/( )\())(    (()/( )\())
    ((_)\ ( )(_))(_)()\ /(_))/(_))(_)\ )\    /(_))(_)\
     _((_)_(_())(_()((_)_)) (_))  _((_)(_)  (_))  _((_)
    | || |_   _||  \/  | |  |_ _||_  /| __| / __|| || |
    | __ | | |  | |\/| | |__ | |  / / | _| _\__ \| __ |
    |_||_| |_|  |_|  |_|____|___|/___||___(_)___/|_||_|

# About

`htmlize.sh` is  a small shell script  wrapped around `htmlize.el` with some
syntax highligting enhancements for particular emacs modes.

`htmlize.sh` has an extensible syntax  recognition enhancement module that you can introduce new functionalities relatively easily;  e.g., it provides CLHS and MOP symbol linking  capabities for  lisp-mode.  (For further  details you  can check `symbols-lisp-clhs.el` and `symbols-lisp-mop.el` files.)

`htmlize.sh`  uses CSS  classes  for labeling  HTML  elements by  default. For an example CSS configuration, see `syntax.css` in the package.

# Demo

    #
    # htmlize.sh command line arguments.
    #
    $ ./htmlize.sh
    Invalid arguments: nil
    Usage: ./htmlize.sh <INPUT-FILE> <OUTPUT-FILE> [<MODE>]


    #
    # Our target file to be htmlized.
    #
    $ cat /tmp/sbcl-server.lisp
    (defpackage :test (:use :cl :sb-bsd-sockets))
    (in-package :test)

    (deftype octet () '(unsigned-byte 8))

    (defun resolve-hostname (name)
      (cond ((eql name :any)  #(0 0 0 0))
            ((typep name '(vector * 4)) name)
            (t (car (host-ent-addresses (get-host-by-name name))))))
    ...
    ...


    #
    # Run htmlize.sh over sbcl-server.lisp contents. (Pay attention that, emacs
    # deduces buffer mode from file extension; otherwise, we may need to supply it
    # manually to htmlize.sh.)
    #
    $ ./htmlize.sh /tmp/sbcl-server.lisp /tmp/sbcl-server.html
    Loading /home/vy/elisp/htmlize/htmlize.el (source)...
    Loading /home/vy/elisp/htmlize/symbols.el (source)...
    Loading /home/vy/elisp/htmlize/symbols-lisp-mop.el (source)...
    Loading /home/vy/elisp/htmlize/symbols-lisp-clhs.el (source)...
    Fontifying sbcl-server.lisp...
    Fontifying sbcl-server.lisp... (syntactically...)
    Fontifying sbcl-server.lisp... (regexps...)
    Fontifying sbcl-server.lisp... (regexps....)
    Fontifying sbcl-server.lisp... (regexps.....)
    Fontifying sbcl-server.lisp... (regexps......)
    Fontifying sbcl-server.lisp... (regexps.......)
    Fontifying sbcl-server.lisp... (regexps........)
    Fontifying sbcl-server.lisp... (regexps.........)
    Fontifying sbcl-server.lisp... (regexps..........)
    Fontifying sbcl-server.lisp... (regexps...........)
    Fontifying sbcl-server.lisp... (regexps............)
    Fontifying sbcl-server.lisp... (regexps.............)
    Fontifying sbcl-server.lisp... (regexps..............)
    Fontifying sbcl-server.lisp... (regexps...............)
    Fontifying sbcl-server.lisp... (regexps................)
    Fontifying sbcl-server.lisp... (regexps.................)
    Fontifying sbcl-server.lisp... (regexps..................)
    Fontifying sbcl-server.lisp... (regexps...................)
    Fontifying sbcl-server.lisp... (regexps....................)
    Fontifying sbcl-server.lisp... (regexps.....................)
    Fontifying sbcl-server.lisp... (regexps......................)
    Fontifying sbcl-server.lisp... (regexps.......................)
    Fontifying sbcl-server.lisp... (regexps........................)
    Fontifying sbcl-server.lisp... (regexps.........................)
    Wrote /tmp/sbcl-server.html


    #
    # htmlize.sh output
    #
    $ cat /tmp/sbcl-server.lisp
    (<span class="builtin"><a href="http://www.lispworks.com/reference/HyperSpec/Body/m_defpkg.htm">defpackage</a></span> <span class="type">:test</span> (<span class="builtin">:use</span> <span class="builtin">:cl</span> <span class="builtin">:sb-bsd-sockets</span>))

    (<span class="builtin"><a href="http://www.lispworks.com/reference/HyperSpec/Body/m_in_pkg.htm">in-package</a></span> <span class="builtin">:test</span>)

    (<span class="builtin"><a href="http://www.lispworks.com/reference/HyperSpec/Body/m_deftp.htm">deftype</a></span> <span class="type">octet</span> () '(<span class="type"><a href="http://www.lispworks.com/reference/HyperSpec/Body/t_unsgn_.htm">unsigned-byte</a></span> 8))

    (<span class="builtin"><a href="http://www.lispworks.com/reference/HyperSpec/Body/m_defun.htm">defun</a></span> <span class="function-name">resolve-hostname</span> (name)
      (<span class="builtin"><a href="http://www.lispworks.com/reference/HyperSpec/Body/m_cond.htm">cond</a></span> ((<span class="builtin"><a href="http://www.lispworks.com/reference/HyperSpec/Body/a_eql.htm">eql</a></span> name <span class="builtin">:any</span>)  #(0 0 0 0))
            ((<span class="builtin"><a href="http://www.lispworks.com/reference/HyperSpec/Body/f_typep.htm">typep</a></span> name '(<span class="builtin"><a href="http://www.lispworks.com/reference/HyperSpec/Body/a_vector.htm">vector</a></span> <span class="variable-name"><a href="http://www.lispworks.com/reference/HyperSpec/Body/a_st.htm">*</a></span> 4)) name)
            (<span class="builtin"><a href="http://www.lispworks.com/reference/HyperSpec/Body/a_t.htm">t</a></span> (<span class="builtin"><a href="http://www.lispworks.com/reference/HyperSpec/Body/f_car_c.htm">car</a></span> (host-ent-addresses (get-host-by-name name))))))
    ...
    ...
