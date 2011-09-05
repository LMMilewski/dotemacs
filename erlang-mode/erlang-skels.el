0;;
;; %CopyrightBegin%
;;
;; Copyright Ericsson AB 2010. All Rights Reserved.
;;
;; The contents of this file are subject to the Erlang Public License,
;; Version 1.1, (the "License"); you may not use this file except in
;; compliance with the License. You should have received a copy of the
;; Erlang Public License along with this software. If not, it can be
;; retrieved online at http://www.erlang.org/.
;;
;; Software distributed under the License is distributed on an "AS IS"
;; basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
;; the License for the specific language governing rights and limitations
;; under the License.
;;
;; %CopyrightEnd%
;;;
;;; Purpose: Provide Erlang code skeletons.
;;; See 'erlang-skel-file' variable.

(defvar erlang-tempo-tags nil
  "Tempo tags for erlang mode")

(defvar erlang-skel
  '(("server (gen_server)" "gen-server" erlang-skel-generic-server erlang-skel-header)
    ("event (gen_event)" "gen-event" erlang-skel-gen-event erlang-skel-header)
    ("fsm (gen_fsm)" "gen-fsm" erlang-skel-gen-fsm erlang-skel-header)
    ("supervisor" "supervisor" erlang-skel-supervisor erlang-skel-header)
    ("application" "application" erlang-skel-application erlang-skel-header)
    ("small server"   "small-server" erlang-skel-small-server erlang-skel-header))
  "*Description of all skeleton templates.
Both functions and menu entries will be created.

Each entry in `erlang-skel' should be a list with three or four
elements, or the empty list.

The first element is the name which shows up in the menu.  The second
is the `tempo' identifier (The string \"erlang-\" will be added in
front of it).  The third is the skeleton descriptor, a variable
containing `tempo' attributes as described in the function
`tempo-define-template'.  The optional fourth elements denotes a
function which should be called when the menu is selected.

Functions corresponding to every template will be created.  The name
of the function will be `tempo-template-erlang-X' where `X' is the
tempo identifier as specified in the second argument of the elements
in this list.

A list with zero elements means that the a horizontal line should
be placed in the menu.")

(defvar erlang-skel-use-separators t
  "A boolean than determines whether the skeletons include horizontal
separators.

Should this variable be nil, the documentation for functions will not
include separators of the form %%--...")

(defvar erlang-skel-header
  '(o "%%%" n
      "%%%" p n
      "%%%" n
      "-module(" (erlang-get-module-from-file-name)  ")." n )
  "*The template of a large header.
Please see the function `tempo-define-template'.")


(defvar erlang-skel-small-server
  '((erlang-skel-include erlang-skel-header)
    "-export([start/0, init/1])." n
    n
    "start() ->" n> "spawn(" (erlang-get-module-from-file-name)
    ", init, [self()])." n
    n
    "init(From) ->" n>
    "loop(From)." n n
    "loop(From) ->" n>
    "receive" n>
    p "_ ->" n>
    "loop(From)" n>
    "end." n)
  "*Template of a small server.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-application
  '((erlang-skel-include erlang-skel-header)
    "-behaviour(application)." n
    n
    "%% Application callbacks" n
    "-export([start/2, stop/1])." n
    n
    (erlang-skel-separator-start 3)
    "%%% Application callbacks" n
    (erlang-skel-separator-end 3)
    "start(_StartType, _StartArgs) ->" n>
    "SUPERVISOR_NAME:start_link()." n
    n
    "stop(_State) ->" n>
    "ok." n
    n
    (erlang-skel-separator-start 3)
    "%%% Internal functions" n
    (erlang-skel-separator-end 3)
    )
  "*The template of an application behaviour.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-supervisor
  '((erlang-skel-include erlang-skel-header)
    "-behaviour(supervisor)." n
    n
    "%% API" n
    "-export([start_link/0])." n
    n
    "%% Supervisor callbacks" n
    "-export([init/1])." n

    n
    (erlang-skel-separator-start 3)
    "%%% API functions" n
    (erlang-skel-separator-end 3)
    "start_link() ->" n>
    "supervisor:start_link({local, ?MODULE}, ?MODULE, [])." n
    n
    (erlang-skel-separator-start 3)
    "%%% Supervisor callbacks" n
    (erlang-skel-separator-end 3)
    "init([]) ->" n>
    "supervisor:start_link({local, ?MODULE}, ?MODULE," n>
    "[" n>
    "])."n
    (erlang-skel-separator-start 3)
    "%%% Internal functions" n
    (erlang-skel-separator-end 3)
    )
  "*The template of an supervisor behaviour.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-generic-server
  '((erlang-skel-include erlang-skel-header)
    "-behaviour(gen_server)." n
    n
    "%% API" n
    "-export([start_link/0])." n
    n
    "%% gen_server callbacks" n
    "-export([init/1, handle_call/3, handle_cast/2, "
    "handle_info/2," n>
    "terminate/2, code_change/3])." n
    n
    "-define(SERVER, ?MODULE). " n
    n
    "-record(state, {})." n
    n
    (erlang-skel-separator-start 3)
    "%%% API" n
    (erlang-skel-separator-end 3)
    "start_link() ->" n>
    "gen_server:start_link({local, ?SERVER}, ?MODULE, [], [])." n
    n
    (erlang-skel-separator-start 3)
    "%%% gen_server callbacks" n
    (erlang-skel-separator-end 3)
    "init([]) ->" n>
    "{ok, #state{}}." n
    n
    "handle_call(_Request, _From, State) ->" n>
    "Reply = ok," n>
    "{reply, Reply, State}." n
    n
    "handle_cast(_Msg, State) ->" n>
    "{noreply, State}." n
    n
    "handle_info(_Info, State) ->" n>
    "{noreply, State}." n
    n
    "terminate(_Reason, _State) ->" n>
    "ok." n
    n
    "code_change(_OldVsn, State, _Extra) ->" n>
    "{ok, State}." n
    n
    (erlang-skel-separator-start 3)
    "%%% Internal functions" n
    (erlang-skel-separator-end 3)
    )
  "*The template of a generic server.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-gen-event
  '((erlang-skel-include erlang-skel-header)
    "-behaviour(gen_event)." n
    n
    "%% API" n
    "-export([start_link/0, add_handler/0])." n
    n
    "%% gen_event callbacks" n
    "-export([init/1, handle_event/2, handle_call/2, " n>
    "handle_info/2, terminate/2, code_change/3])." n
    n
    "-define(SERVER, ?MODULE). " n
    n
    "-record(state, {})." n
    n
    (erlang-skel-separator-start 3)
    "%%% gen_event callbacks" n
    (erlang-skel-separator-end 3)
    "start_link() ->" n>
    "gen_event:start_link({local, ?SERVER})." n
    n
    "add_handler() ->" n>
    "gen_event:add_handler(?SERVER, ?MODULE, [])." n
    n
    (erlang-skel-separator-start 3)
    "%%% gen_event callbacks" n
    (erlang-skel-separator-end 3)
    "init([]) ->" n>
    "{ok, #state{}}." n
    n
    "handle_event(_Event, State) ->" n>
    "{ok, State}." n
    n
    "handle_call(_Request, State) ->" n>
    "Reply = ok," n>
    "{ok, Reply, State}." n
    n
    "handle_info(_Info, State) ->" n>
    "{ok, State}." n
    n
    "terminate(_Reason, _State) ->" n>
    "ok." n
    n
    "code_change(_OldVsn, State, _Extra) ->" n>
    "{ok, State}." n
    n
    (erlang-skel-separator-start 3)
    "%%% Internal functions" n
    (erlang-skel-separator-end 3)
    )
  "*The template of a gen_event.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-gen-fsm
  '((erlang-skel-include erlang-skel-header)
    "-behaviour(gen_fsm)." n
    n
    "%% API" n
    "-export([start_link/0])." n
    n
    "%% gen_fsm callbacks" n
    "-export([init/1, state_name/2, state_name/3, handle_event/3," n>
    "handle_sync_event/4, handle_info/3, terminate/3, code_change/4])." n
    n
    "-define(SERVER, ?MODULE)." n
    n
    "-record(state, {})." n
    n
    (erlang-skel-separator-start 3)
    "%%% API" n
    (erlang-skel-separator-end 3)
    "start_link() ->" n>
    "gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], [])." n
    n
    (erlang-skel-separator-start 3)
    "%%% gen_fsm callbacks" n
    (erlang-skel-separator-end 3)
    "init([]) ->" n>
    "{ok, state_name, #state{}}." n
    n
    "state_name(_Event, State) ->" n>
    "{next_state, state_name, State}." n
    n
    "state_name(_Event, _From, State) ->" n>
    "Reply = ok," n>
    "{reply, Reply, state_name, State}." n
    n
    "handle_event(_Event, StateName, State) ->" n>
    "{next_state, StateName, State}." n
    n
    "handle_sync_event(_Event, _From, StateName, State) ->" n>
    "Reply = ok," n>
    "{reply, Reply, StateName, State}." n
    n
    "handle_info(_Info, StateName, State) ->" n>
    "{next_state, StateName, State}." n
    n
    "terminate(_Reason, _StateName, _State) ->" n>
    "ok." n
    n
    "code_change(_OldVsn, StateName, State, _Extra) ->" n>
    "{ok, StateName, State}." n
    n
    (erlang-skel-separator-start 3)
    "%%% Internal functions" n
    (erlang-skel-separator-end 3)
    )
  "*The template of a gen_fsm.
Please see the function `tempo-define-template'.")

;; Skeleton code:

;; This code is based on the package `tempo' which is part of modern
;; Emacsen.  (GNU Emacs 19.25 (?) and XEmacs 19.14.)

(defun erlang-skel-init ()
  "Generate the skeleton functions and menu items.
The variable `erlang-skel' contains the name and descriptions of
all skeletons.

The skeleton routines are based on the `tempo' package.  Should this
package not be present, this function does nothing."
  (interactive)
  (condition-case nil
      (require 'tempo)
    (error t))
  (if (featurep 'tempo)
      (let ((skel erlang-skel)
            (menu '()))
        (while skel
          (cond ((null (car skel))
                 (setq menu (cons nil menu)))
                (t
                 (funcall (symbol-function 'tempo-define-template)
                          (concat "erlang-" (nth 1 (car skel)))
                          ;; The tempo template used contains an `include'
                          ;; function call only, hence changes to the
                          ;; variables describing the templates take effect
                          ;; immdiately.
                          (list (list 'erlang-skel-include (nth 2 (car skel))))
                          (nth 1 (car skel)))
                 (setq menu (cons (erlang-skel-make-menu-item
                                   (car skel)) menu))))
          (setq skel (cdr skel)))
        (setq erlang-menu-skel-items
              (list nil (list "Skeletons" (nreverse menu))))
        (setq erlang-menu-items
              (erlang-menu-add-above 'erlang-menu-skel-items
                                     'erlang-menu-version-items
                                     erlang-menu-items))
        (erlang-menu-init))))

(defun erlang-skel-make-menu-item (skel)
  (let ((func (intern (concat "tempo-template-erlang-" (nth 1 skel)))))
    (cond ((null (nth 3 skel))
           (list (car skel) func))
          (t
           (list (car skel)
                 (list 'lambda '()
                       '(interactive)
                       (list 'funcall
                             (list 'quote (nth 3 skel))
                             (list 'quote func))))))))

;; Functions designed to be added to the skeleton menu.
;; (Not normally used)
(defun erlang-skel-insert (func)
  "Insert skeleton generated by FUNC and goto first tempo mark."
  (save-excursion (funcall func))
  (funcall (symbol-function 'tempo-forward-mark)))

(defun erlang-skel-header (func)
  "Insert the header generated by FUNC at the beginning of the buffer."
  (goto-char (point-min))
  (save-excursion (funcall func))
  (funcall (symbol-function 'tempo-forward-mark)))


;; Functions used inside the skeleton descriptions.
(defun erlang-skel-skip-blank ()
  (skip-chars-backward " \t")
  nil)

(defun erlang-skel-include (&rest args)
  "Include a template inside another template.

Example of use, assuming that `erlang-skel-func' is defined:

 (defvar foo-skeleton '(\"%%% New function:\"
                        (erlang-skel-include erlang-skel-func)))

Technically, this function returns the `tempo' attribute`(l ...)' which
can contain other `tempo' attributes.  Please see the function
`tempo-define-template' for a description of the `(l ...)' attribute."
  (let ((res '())
        entry)
    (while args
      (setq entry (car args))
      (while entry
        (setq res (cons (car entry) res))
        (setq entry (cdr entry)))
      (setq args (cdr args)))
    (cons 'l (nreverse res))))

(defun erlang-skel-separator (&optional percent)
  "Return a double line (equals sign) comment separator."
  (let ((percent (or percent 3)))
    (concat (make-string percent ?%)
            (make-string (- 70 percent) ?=)
            "\n")))

(defun erlang-skel-separator-start (&optional percent)
  "Return a double separator or a newline if separators are configured off."
  (if erlang-skel-use-separators
      (erlang-skel-separator percent)
    "\n"))

(defun erlang-skel-separator-end (&optional percent)
  "Return a double separator or an empty string if separators are
configured off."
  (if erlang-skel-use-separators
      (erlang-skel-separator percent)
    ""))

;; Local variables:
;; coding: iso-8859-1
;; End:

;;; erlang-skels.el ends here
