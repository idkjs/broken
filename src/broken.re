/**  Broken -- Unit testing

   Broken (https://github.com/michipili/broken)
   This file is part of Broken

   Copyright © 2014–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt */
open Printf;

let exit_success = 0;
let exit_failure = 1;
let exit_usage = 64;
let exit_unavailable = 69;
let exit_software = 70;

/* Timestamp */
let timestamp = s => {
  open Unix;
  let day_of_week = n =>
    switch (n) {
    | 0 => "Sun"
    | 1 => "Mon"
    | 2 => "Tue"
    | 3 => "Wed"
    | 4 => "Thu"
    | 5 => "Fri"
    | 6 => "Sat"
    | _ => invalid_arg("day_of_week")
    };

  let month_of_year = n =>
    switch (n) {
    | 0 => "Jan"
    | 1 => "Feb"
    | 2 => "Mar"
    | 3 => "Apr"
    | 4 => "May"
    | 5 => "Jun"
    | 6 => "Jul"
    | 7 => "Aug"
    | 8 => "Sep"
    | 9 => "Oct"
    | 10 => "Nov"
    | 11 => "Dec"
    | _ => invalid_arg("month_of_year")
    };

  sprintf(
    "%s %s %2d %02d:%02d:%02d %04d",
    day_of_week(s.tm_wday),
    month_of_year(s.tm_mon),
    s.tm_mday,
    s.tm_hour,
    s.tm_min,
    s.tm_sec,
    1900 + s.tm_year,
  );
};

let curr_timestamp = () => timestamp(Unix.gmtime(Unix.time()));

/* Misc */

let expected_sz = 42; /* Expected size of our set of test cases */

let path_cat = (p1, p2) =>
  /* Concatenation of test path elements */
  if (p1 == "") {
    p2;
  } else {
    p1 ++ "." ++ p2;
  };

let equal_float = (~epsilon=epsilon_float, x, y) =>
  abs_float(x -. y) <= epsilon *. abs_float(max(x, y));

/* Test case */

type t = {
  ident: string,
  predicate: unit => bool,
  expected_failure: bool,
};

type printer('a) = (Format.formatter, 'a) => unit;

/* Creating tests */

let make_case = (ident, ~expected_failure=false, predicate) => {
  ident,
  predicate,
  expected_failure,
};

let maybe_print_value = (printer, header, value) => {
  let wrap_printer = (p, out_channel, x) => {
    open Format;
    let formatter = formatter_of_out_channel(out_channel);
    p(formatter, x);
    pp_print_flush(formatter, ());
  };

  switch (printer) {
  | Some(p) => printf("%s: %a\n", header, wrap_printer(p), value)
  | None => ()
  };
};

let assert_equal =
    (ident, ~expected_failure=?, ~printer=?, ~equal=(==), f, x, y) => {
  let maybe_print_log = (expected, got) => {
    maybe_print_value(printer, "Test-Expected", expected);
    maybe_print_value(printer, "Test-Got", got);
  };

  let predicate = () => {
    let expected = y;
    let got = f(x);
    equal(expected, got)
    || {
      maybe_print_log(expected, got);
      false;
    };
  };

  make_case(ident, ~expected_failure?, predicate);
};

let assert_success = ident => make_case(ident, _ => true);

let assert_failure = ident => make_case(ident, _ => false);

let assert_true = (ident, ~expected_failure=?, f, x) =>
  make_case(ident, ~expected_failure?, _ => f(x));

let assert_false = (ident, ~expected_failure=?, f, x) =>
  make_case(ident, ~expected_failure?, _ => !f(x));

let assert_for_all = (ident, ~expected_failure=?, ~printer=?, p, l) => {
  let wrap_predicate = (p, x) =>
    p(x)
    || {
      maybe_print_value(printer, "Test-For-All", x);
      false;
    };

  make_case(ident, ~expected_failure?, _ =>
    List.for_all(wrap_predicate(p), l)
  );
};

let assert_exists = (ident, ~expected_failure=?, ~printer=?, p, l) => {
  let wrap_predicate = (p, x) =>
    p(x)
    && {
      maybe_print_value(printer, "Test-Exists", x);
      true;
    };

  make_case(ident, ~expected_failure?, _ =>
    List.exists(wrap_predicate(p), l)
  );
};

let assert_string = (ident, ~expected_failure=?, f, x, y) =>
  assert_equal(
    ident,
    ~expected_failure?,
    ~printer=Format.pp_print_string,
    f,
    x,
    y,
  );

let assert_int = (ident, ~expected_failure=?, f, x, y) =>
  assert_equal(
    ident,
    ~expected_failure?,
    ~printer=Format.pp_print_int,
    f,
    x,
    y,
  );

let assert_zero = (ident, ~expected_failure=?, f, x) =>
  assert_equal(
    ident,
    ~expected_failure?,
    ~printer=Format.pp_print_int,
    f,
    x,
    0,
  );

let assert_nonzero = (ident, ~expected_failure=?, f, x) =>
  assert_equal(
    ident,
    ~expected_failure?,
    ~printer=Format.pp_print_int,
    ~equal=(!==), /* Yes, it is perverse. */
    f,
    x,
    0,
  );

let assert_exception = (ident, ~expected_failure=?, e, f, x) =>
  make_case(ident, ~expected_failure?, _ =>
    try(
      {
        ignore(f(x));
        false;
      }
    ) {
    | except when except == e => true
    }
  );

let assert_float = (ident, ~expected_failure=?, y, f, x) =>
  assert_equal(
    ident,
    ~expected_failure?,
    ~printer=Format.pp_print_float,
    ~equal=equal_float,
    y,
    f,
    x,
  );

let float_to_precise_string = (n, x) => sprintf("%.*f", n, x);

let equal_precision = (n, x, y) =>
  float_to_precise_string(n, x) == float_to_precise_string(n, y);

let assert_precision = (label, ~expected_failure=?, n, f, x, y) =>
  assert_equal(
    ~expected_failure?,
    ~equal=equal_precision(n),
    label,
    f,
    x,
    y,
  );

/* Compound test cases */

let case_prepend = (prefix, t) => {...t, ident: path_cat(prefix, t.ident)};

let case_combinator = (loop, ident, ~expected_failure=?, list) =>
  make_case(ident, ~expected_failure?, _ =>
    loop(t => t.predicate(), List.map(case_prepend(ident), list))
  );

let exists = case_combinator(List.exists);

let for_all = case_combinator(List.for_all);

let ident = t => t.ident;

/* Test fixtures */

type fixture = {
  setup: unit => unit,
  tear_down: unit => unit,
};

let make_fixture = (setup, tear_down) => {setup, tear_down};

let supervise_fixture = (fixture, f, x) => {
  let supervise = action =>
    try(action()) {
    | exn =>
      eprintf("UnitTest: fixture exception: %s\n", Printexc.to_string(exn));
      exit(exit_unavailable);
    };

  let _ = supervise(fixture.setup);
  let a = f(x);
  let _ = supervise(fixture.tear_down);
  a;
};

let relax = make_fixture(ignore, ignore);

let pr_generator = Random.State.make_self_init();

let pr_name = (prefix, suffix) => {
  let pr_number = Random.State.bits(pr_generator) land 0xFFFFFF;
  let pr_basename = sprintf("%s%06x%s", prefix, pr_number, suffix);
  Filename.concat(Filename.get_temp_dir_name(), pr_basename);
};

let tmpfile = (prefix, suffix, r) => {
  let sname = ref("\000"); /* An invalid filename */
  let setup = () => {
    sname := Filename.temp_file(prefix, suffix);
    r := sname^;
  };

  let tear_down = () => Unix.unlink(sname^);
  {setup, tear_down};
};

let tmpdir_setup = (scwd, sname, prefix, suffix, r, ()) => {
  let tmpdir_max = 100; /* Max attempts */
  let rec tmpdir_try = counter => {
    let name = pr_name(prefix, suffix);
    try(
      {
        Unix.mkdir(name, 0o700);
        r := name;
        sname := name;
        scwd := Unix.getcwd();
        try(Unix.chdir(name)) {
        | Sys_error(_) as e =>
          Unix.chdir(scwd^);
          raise(e);
        };
      }
    ) {
    | Sys_error(_) as e =>
      if (counter >= tmpdir_max) {
        raise(e);
      } else {
        tmpdir_try(counter + 1);
      }
    };
  };
  tmpdir_try(0);
};

let rec rmRf = path => {
  let dir = Unix.opendir(path);
  try(
    while (true) {
      let base = Unix.readdir(dir);
      let full = Filename.concat(path, base);
      let stat = Unix.lstat(full);
      switch (base, stat.Unix.st_kind) {
      | (".", _)
      | ("..", _) => ()
      | (_, Unix.S_REG)
      | (_, Unix.S_LNK)
      | (_, Unix.S_FIFO)
      | (_, Unix.S_SOCK) => Unix.unlink(full)
      | (_, Unix.S_DIR) => rmRf(full)
      | (_, Unix.S_CHR)
      | (_, Unix.S_BLK) => ()
      };
    }
  ) {
  | End_of_file =>
    Unix.closedir(dir);
    Unix.rmdir(path);
  };
};

let tmpdir_tear_down = (scwd, sname, ()) => {
  Unix.chdir(scwd^);
  rmRf(sname^);
};

let tmpdir = (prefix, suffix, reference) => {
  let scwd = ref("\000"); /* An invalid filename */
  let sname = ref("\000"); /* An invalid filename */
  {
    setup: tmpdir_setup(scwd, sname, prefix, suffix, reference),
    tear_down: tmpdir_tear_down(scwd, sname),
  };
};

/* Outcome */

type outcome =
  | Success
  | Failure
  | Not_implemented
  | Skipped
  | Exception(exn);

let outcome_to_char =
  fun
  | Success => ' '
  | Failure => '~'
  | Not_implemented => '?'
  | Skipped => '>'
  | Exception(_) => '!';

let outcome_to_string = (ident, outcome, expected_failure) => {
  let c = outcome_to_char(outcome);
  let x = if (expected_failure) {" (expected failure)"} else {""};
  switch (outcome) {
  | Exception(exn) =>
    sprintf("%c %s %s%s", c, ident, Printexc.to_string(exn), x)
  | _ => sprintf("%c %s%s", c, ident, x)
  };
};

let outcome_describe =
  fun
  | Success => "succeed"
  | Failure => "failed"
  | Not_implemented => "not implemented"
  | Skipped => "skept"
  | Exception(exc) => sprintf("raised %s", Printexc.to_string(exc));

let outcome_is_successful =
  fun
  | Success
  | Skipped => true
  | _ => false;

let is_expected_failure = (case, outcome) =>
  !outcome_is_successful(outcome) && case.expected_failure;

/* Test suites */

type suite_item =
  | Case(fixture, t)
  | Suite(fixture, suite)
and suite = {
  suite_ident: string,
  suite_description: string,
  suite_fixture: fixture,
  suite_queue: Queue.t(suite_item),
};

let make_suite = (~fixture=relax, ident, description) => {
  suite_ident: ident,
  suite_description: description,
  suite_fixture: fixture,
  suite_queue: Queue.create(),
};

let add_case = (~fixture=relax, s, case) =>
  Queue.add([@implicit_arity] Case(fixture, case), s.suite_queue);

let add_suite = (~fixture=relax, s, suite) =>
  Queue.add([@implicit_arity] Suite(fixture, suite), s.suite_queue);

/* Supervisor */

type message =
  | NOT_IMPLEMENTED
  | SKIP;

exception Message(message);

let message = m => raise(Message(m));

let not_implemented = () => message(NOT_IMPLEMENTED);

let skip_if = p =>
  if (p) {
    message(SKIP);
  };

let only_for = p => skip_if(!p);

let run_case = t =>
  /* The outcome of a test case */
  try(
    if (t.predicate()) {
      Success;
    } else {
      Failure;
    }
  ) {
  | Message(NOT_IMPLEMENTED) => Not_implemented
  | Message(SKIP) => Skipped
  | other => Exception(other)
  };

class type supervisor = {
  pub case_begin: (string, t) => unit;
  pub case_end: (string, t) => unit;
  pub case_outcome: (string, t, outcome) => unit;
  pub case_run: (string, fixture, t) => bool;
  pub root_begin: (string, unit) => unit;
  pub root_end: (string, unit) => unit;
  pub root_run: suite => bool;
  pub suite_begin: (string, suite) => unit;
  pub suite_end: (string, suite) => unit;
  pub suite_run: (string, fixture, suite) => bool;
  pub summary: unit => unit;
};

class virtual meta_supervisor = {
  let root_memoize = Hashtbl.create(expected_sz);
  as self;
  pub virtual root_begin: (string, unit) => unit;
  pub virtual root_end: (string, unit) => unit;
  pub virtual suite_begin: (string, suite) => unit;
  pub virtual suite_end: (string, suite) => unit;
  pub virtual case_begin: (string, t) => unit;
  pub virtual case_outcome: (string, t, outcome) => unit;
  pub virtual case_end: (string, t) => unit;
  pub virtual summary: unit => unit;
  pri really_suite_run = (ident, setup, s, tear_down) => {
    let is_true = b => b;
    let _ = s.suite_fixture.setup();
    let _ = setup();
    let l = List.rev(Queue.fold((a, x) => [x, ...a], [], s.suite_queue));
    let x = List.map(self#suite_item_run(ident), l);
    let a = List.for_all(is_true, x);
    let _ = tear_down();
    let _ = s.suite_fixture.tear_down();
    a;
  };
  pri really_root_run = s => {
    let n = s.suite_ident;
    let a =
      self#really_suite_run(n, self#root_begin(n), s, self#root_end(n));
    self#summary();
    a;
  };
  pub suite_run = (ident, f, s) => {
    let () = self#suite_begin(ident, s);
    let answer =
      self#really_suite_run(
        path_cat(ident, s.suite_ident),
        f.setup,
        s,
        f.tear_down,
      );

    let () = self#suite_end(ident, s);
    answer;
  };
  pub root_run = s =>
    try(Hashtbl.find(root_memoize, s.suite_ident)) {
    | Not_found => self#really_root_run(s)
    };
  pri suite_item_run = (ident, item) =>
    switch (item) {
    | [@implicit_arity] Case(f, c) => self#case_run(ident, f, c)
    | [@implicit_arity] Suite(f, s) => self#suite_run(ident, f, s)
    };
  pub case_run = (ident, fixture, c) => {
    let path = path_cat(ident, c.ident);
    let _ = self#case_begin(path, c);
    let o = supervise_fixture(fixture, run_case, c);
    let _ = self#case_outcome(path, c, o);
    let _ = self#case_end(path, c);
    outcome_is_successful(o) || c.expected_failure;
  };
};

class outcome_memoizer = {
  let outcome_memoize: Queue.t((string, outcome, bool)) = (
    Queue.create(): Queue.t((string, outcome, bool))
  );
  as self;
  pub reset = Queue.clear(outcome_memoize);
  pub push = (ident, outcome, expected_failure) =>
    Queue.add((ident, outcome, expected_failure), outcome_memoize);
  pub get = List.rev(Queue.fold((a, x) => [x, ...a], [], outcome_memoize));
}; /* Keeping track of outcomes */

class verbose_supervisor: supervisor = {
  let memo = new outcome_memoizer;
  as _;
  inherit class meta_supervisor;
  pub root_begin = (ident, ()) => {
    memo#reset;
    printf("===>  Begin test suite %s\n", ident);
  };
  pub root_end = (ident, ()) => printf("===>  End test suite %s\n", ident);
  pub suite_begin = (_, _) => ();
  pub suite_end = (_, _) => ();
  pub case_begin = (ident, case) => printf("=>  Test case %s\n", ident);
  pub case_outcome = (ident, case, outcome) => {
    memo#push(path_cat(ident, case.ident), outcome, case.expected_failure);
    printf(
      "=> Test case %s %s%s\n",
      ident,
      outcome_describe(outcome),
      if (is_expected_failure(case, outcome)) {
        " (expected failure)";
      } else {
        "";
      },
    );
  };
  pub case_end = (ident, case) => ();
  pub summary = () => {
    flush(stdout);
    List.iter(
      ((i, o, x)) => prerr_endline(outcome_to_string(i, o, x)),
      memo#get,
    );
  };
};

class concise_supervisor: supervisor = {
  let memo = new outcome_memoizer;
  let sep = String.make(70, '-');
  let prerr_sep = () => prerr_endline(sep);
  as self;
  inherit class meta_supervisor;
  val fd_stdout = Unix.dup(Unix.stdout);
  val fd_stderr = Unix.dup(Unix.stderr);
  val stat_root_start = ref(0.);
  val stat_root_stop = ref(0.);
  val stat_case_count = ref(0);
  pri logfile_begin = file => {
    let logfile = open_out(file);

    List.iter(flush, [stdout, stderr]);
    Unix.dup2(Unix.descr_of_out_channel(logfile), Unix.stdout);
    Unix.dup2(fd_stdout, Unix.stderr);
    close_out(logfile);
  };
  pri logfile_end = () => {
    List.iter(flush, [stdout, stderr]);
    Unix.dup2(fd_stdout, Unix.stdout); /* Closes logfile bound to stdout */
    Unix.dup2(fd_stderr, Unix.stderr);
  };
  pub root_begin = (ident, ()) => {
    stat_root_start := Sys.time();
    stat_case_count := 0;
    memo#reset;
    self#logfile_begin(ident ++ ".log");
    eprintf("Test suite %s\n", ident);
  };
  pub root_end = (ident, ()) => {
    stat_root_stop := Sys.time();
    prerr_newline();
    self#logfile_end();
  };
  pub suite_begin = (_, _) => ();
  pub suite_end = (_, _) => ();
  pub case_begin = (ident, case) => {
    incr(stat_case_count);
    printf("From BROKEN %s\n", curr_timestamp());
    printf("Test-Case: %s\n", ident);
  };
  pub case_outcome = (ident, case, outcome) => {
    let outcome_brief =
      switch (outcome_to_char(outcome)) {
      | ' ' => '.'
      | x => x
      };

    printf("Test-Outcome-Brief: %c\n", outcome_brief);
    printf("Test-Outcome: %s\n", outcome_describe(outcome));
    if (is_expected_failure(case, outcome)) {
      printf("Test-Outcome-Expected-Failure: yes\n");
    };
    printf("\n");
    memo#push(path_cat(ident, case.ident), outcome, case.expected_failure);
    prerr_char(outcome_brief);
  };
  pub case_end = (ident, case) => printf("\n");
  pub summary = () => {
    let prerr_outcome = ((i, o, x)) =>
      if (!outcome_is_successful(o)) {
        prerr_endline(outcome_to_string(i, o, x));
      };

    flush(stdout);
    flush(stderr);
    prerr_sep();
    eprintf(
      "Ran %d tests in %.3fs\n\n%!",
      stat_case_count^,
      stat_root_stop^ -. stat_root_start^,
    );
    List.iter(prerr_outcome, memo#get);
  };
};

let verbose = new verbose_supervisor;
let concise = new concise_supervisor;

/* Test suite interface */

type root = {
  root_suite: suite,
  root_prerequisite: list(string),
};

let root_registry = Hashtbl.create(expected_sz);

let register = suite =>
  Hashtbl.add(
    root_registry,
    suite.suite_ident,
    {root_suite: suite, root_prerequisite: []},
  );

let mem = Hashtbl.mem(root_registry);

let register_suite = (~fixture=?, ident, description, lst) => {
  let s = make_suite(~fixture?, ident, description);
  List.iter(add_case(s), lst);
  register(s);
};

let (|&) = (s, c) => {
  add_case(s, c);
  s;
};

let (|@) = (s, lst) => {
  List.iter(add_case(s), lst);
  s;
};

let ( |* ) = (s, suite) => {
  add_suite(s, suite);
  s;
};

let (|:) = (s, lst) => {
  List.iter(add_suite(s), lst);
  s;
};

module Ident = Set.Make(String);

let set_of_list = l => List.fold_right(Ident.add, l, Ident.empty);

let set_to_list = s => Ident.fold((h, t) => [h, ...t], s, []);

let generic_list = loop => {
  let sort = List.sort(String.compare);
  let root_registry_keys = Hashtbl.fold(loop, root_registry, Ident.empty);
  sort(set_to_list(root_registry_keys));
};

let list_suites = () => {
  let loop = (ident, _, a) => Ident.add(ident, a);
  generic_list(loop);
};

let list_expected_failures = () => {
  let rec subloop = (prefix, a, item) => {
    let conditionally_add_case = (case, a) =>
      if (case.expected_failure) {
        Ident.add(path_cat(prefix, case.ident), a);
      } else {
        a;
      };

    switch (item) {
    | [@implicit_arity] Case(_, c) => conditionally_add_case(c, a)
    | [@implicit_arity] Suite(_, suite) =>
      let ident = path_cat(prefix, suite.suite_ident);
      Queue.fold(subloop(ident), a, suite.suite_queue);
    };
  };

  let loop = (ident, root, a) =>
    Queue.fold(subloop(ident), a, root.root_suite.suite_queue);

  generic_list(loop);
};

let rec toposort =
        (
          n, /* Number of elements in u at the beginning of the last iteration */
          l, /* List of sorted identifiers */
          a, /* Set of sorted identifiers, it has the same elements as l */
          u, /* Dependencies to go */
          v,
        ) =>
  /* Postponed dependencies */
  switch (u) {
  | [] when v == [] => l
  | [] when List.length(v) == n => failwith("toposort")
  | [] => toposort(List.length(v), l, a, v, [])
  | [(ident, deps), ...t] when Ident.subset(deps, a) =>
    toposort(n, [ident, ...l], Ident.add(ident, a), t, v)
  | [h, ...t] => toposort(n, l, a, t, [h, ...v])
  };

let direct_dependencies = ident => {
  let deps = r => set_of_list(r.root_prerequisite);
  let l = Hashtbl.find_all(root_registry, ident);
  List.fold_left(Ident.union, Ident.empty, List.map(deps, l));
};

let rec deep_dependencies =
        (
          a, /* Final dependencies */
          b,
        ) =>
  /* Dependencies to explore */
  if (Ident.subset(b, a)) {
    a;
  } else {
    let x = Ident.choose(b);
    let d = direct_dependencies(x);
    deep_dependencies(deep_dependencies(a, d), Ident.remove(x, b));
  };

let dependencies = ident =>
  set_to_list(
    deep_dependencies(Ident.empty, Ident.add(ident, Ident.empty)),
  );

module Application = {
  type parameter = {
    verbose: bool,
    action,
  }
  and action =
    | Help
    | Usage
    | ListExpectedFailures
    | ListAvailable
    | RunAll
    | RunList(list(string));

  let progname = () => Filename.basename(Sys.executable_name);

  let usage = () =>
    sprintf(
      "Usage: %s [-h | -l | -x | suite1 [suite2 [...]]]\n Run unitary tests\nOptions:",
      progname(),
    );

  let prerr_usage = () =>
    eprintf(
      "Usage: %s [-h | -l | -x | suite1 [suite2 [...]]]\n Run unitary tests\n",
      progname(),
    );

  let prerr_help = () => {
    prerr_usage();
    eprintf(
      "Options:\n -h Display a cheerful help message.\n -l List available test suites.\n -x List all test suites marked as expected failures.\nExit Status:\n The %s program exits 0 on success and 1 if a test case failed.\n",
      progname(),
    );
  };

  let parse = () => {
    let action = ref(RunAll);
    let testcases = ref([]);
    let add = case => testcases := [case, ...testcases^];

    let spec = [
      (
        "-h",
        Arg.Unit(() => action := Help),
        "Display a cheerful help message.",
      ),
      (
        "-l",
        Arg.Unit(() => action := ListAvailable),
        "List available test suites.",
      ),
      (
        "-x",
        Arg.Unit(() => action := ListExpectedFailures),
        "List all test suites marked as expected failures.",
      ),
      (
        "-help",
        Arg.Unit(() => raise(Arg.Bad("unknown option '-help'"))),
        "",
      ),
      (
        "--help",
        Arg.Unit(() => raise(Arg.Bad("unknown option '--help'"))),
        "",
      ),
    ];

    try(Arg.parse(spec, add, usage())) {
    | Arg.Help(_) => action := Help
    | Arg.Bad(_) => action := Usage
    };
    switch (action^, testcases^) {
    | (RunAll, []) => ()
    | (RunAll, lst) => action := RunList(List.rev(lst))
    | (_, []) => ()
    | (_, _) => action := Usage
    };
    {
      verbose:
        try(
          {
            ignore(Sys.getenv("BROKEN_VERBOSE"));
            true;
          }
        ) {
        | Not_found => false
        },
      action: action^,
    };
  };

  let rec run = (supervisor, ident) => {
    let supervise = r => supervisor#root_run(r.root_suite);

    if (mem(ident)) {
      List.for_all(run(supervisor), dependencies(ident))
      && List.for_all(supervise, Hashtbl.find_all(root_registry, ident));
    } else {
      eprintf("UnitTest: run: test suite not found: %s", ident);
      exit(exit_software);
    };
  };

  let run_several = (supervisor, lst) => {
    let is_true = x => x;
    /* Recall that List.for_all is a short-cut and operator */
    List.for_all(is_true, List.map(run(supervisor), lst));
  };

  let run_all = supervisor => run_several(supervisor, list_suites());

  let main = () => {
    let param = parse();
    let supervisor =
      if (param.verbose) {
        verbose;
      } else {
        concise;
      };

    switch (param.action) {
    | Help =>
      prerr_help();
      exit(0);
    | Usage =>
      prerr_usage();
      exit(64);
    | ListExpectedFailures =>
      List.iter(print_endline, list_expected_failures());
      exit(0);
    | ListAvailable =>
      List.iter(print_endline, list_suites());
      exit(0);
    | RunAll =>
      if (run_all(supervisor)) {
        exit(0);
      } else {
        exit(1);
      }
    | RunList(lst) =>
      if (run_several(supervisor, lst)) {
        exit(0);
      } else {
        exit(1);
      }
    };
  };
};

let main = () => Application.main();
