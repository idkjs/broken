/*  Broken -- Unit testing

    Broken (https://github.com/michipili/broken)
    This file is part of Broken

    Copyright © 2014–2015 Michael Grünewald

    This file must be used under the terms of the CeCILL-B.
    This source file is licensed as described in the file COPYING, which
    you should have received as part of this distribution. The terms
    are also available at
    http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt */

/** Unit testing.

This {i unit test framework} supports {i test automation, sharing
of setup and teardown code} for tests and {i aggregation of tests in
collections}.

To achieve this, we support some important concepts:

{b Test case.}
A test case is the smallest unit of testing.  It usually compares an
expected behaviour with the actual behaviour for a particular set of
inputs.


{b Test outcome.}
A test outcome represents the result of a test case, {i i.e.} if the
actual behaviour matches the expected behaviour.


{b Test fixture.}
A test fixture represents the preparation needed to perform one or
more test cases, and any associated cleanup actions.  This may
involve, for example, creating temporary directories, logging into a
database, or starting a server process.


{b Test suite.}
A test suite is a collection of test cases, test suites, or
both.  It is used to aggregate tests that should be executed
together, for instance, the tests for a given component.
When a test suite is exercised, all the test suites and all
the test cases it contains are exercised.


{b Test supervisor.}
A test supervisor is a component which orchestrates the execution of
tests and provides the outcome to the user.  The supervisor may use a
graphical interface, a textual interface, or return a special value to
indicate the results of executing the tests. */

/** The abstract type of test cases. */

type t;

/** The type of printers for values of type ['a]. */

type printer('a) = (Format.formatter, 'a) => unit;

/** 6 Simple test cases */

/** A test case that always succeeds, the string identifies the test case. */

let assert_success: string => t;

/** A test case that always fails, the string identifies the test case. */

let assert_failure: string => t;

/** [assert_equal ident f x y] creates a test case computing [f x] and comparing
the returned value with [y].  The test case succeeds if these values
are equal and fails otherwise.

Diagnostic messages can be emitted to [stdout], they are gathered
by the test supervisor orchestrating the execution of the test case.
Use of the [Format] module is encouraged.

@param equal Predicate used to compare values (defaults to polymorphic [=]).
@param printer Printer function used to output values.
*/

let assert_equal:
  (
    string,
    ~expected_failure: bool=?,
    ~printer: printer('b)=?,
    ~equal: ('b, 'b) => bool=?,
    'a => 'b,
    'a,
    'b
  ) =>
  t;

/** Specialised version of [assert_equal] for [true]. */

let assert_true: (string, ~expected_failure: bool=?, 'a => bool, 'a) => t;

/** Specialised version of [assert_equal] for [false]. */

let assert_false: (string, ~expected_failure: bool=?, 'a => bool, 'a) => t;

/** [assert_for_all ident predicate list] creates a test case
verifying that all the elements of [list] pass the predicate.

@param printer Printer function used to output values. */

let assert_for_all:
  (
    string,
    ~expected_failure: bool=?,
    ~printer: printer('a)=?,
    'a => bool,
    list('a)
  ) =>
  t;

/** [assert_for_all ident predicate list] creates a test case
verifying that at least one element of [list] passes the predicate.

@param printer Printer function used to output values. */

let assert_exists:
  (
    string,
    ~expected_failure: bool=?,
    ~printer: printer('a)=?,
    'a => bool,
    list('a)
  ) =>
  t;

/** Specialised version of [assert_equal] for strings. */

let assert_string:
  (string, ~expected_failure: bool=?, 'a => string, 'a, string) => t;

/** Specialised version of [assert_equal] for integers. */

let assert_int: (string, ~expected_failure: bool=?, 'a => int, 'a, int) => t;

/** Specialised version of [assert_equal] for [0]. */

let assert_zero: (string, ~expected_failure: bool=?, 'a => int, 'a) => t;

/** A test case similar to [assert_equal], testing for a non zero result. */

let assert_nonzero: (string, ~expected_failure: bool=?, 'a => int, 'a) => t;

/** A test case similar to [assert_equal], testing for a floating
point result being close from an expected value. */

let assert_float:
  (string, ~expected_failure: bool=?, 'a => float, 'a, float) => t;

/** A test case similar to [assert_float], testing for a floating
point result being close from an expected value with a given
precision. */

let assert_precision:
  (string, ~expected_failure: bool=?, int, 'a => float, 'a, float) => t;

/** A test case that succeeds only when the computed function raises the
given exception. */

let assert_exception:
  (string, ~expected_failure: bool=?, exn, 'a => 'b, 'a) => t;

/** 6 Compound test cases */

/** A test case succeeding only if all the tests in the given list do.
The string identifies the test case. */

let for_all: (string, ~expected_failure: bool=?, list(t)) => t;

/** A test case succeeding only if one of the tests in the given list does.
The string identifies the test case. */

let exists: (string, ~expected_failure: bool=?, list(t)) => t;

/** 6 Examining test cases */

/** The string identifying a test case. */

let ident: t => string;

/** 6 Test fixtures */

/** The abstract type of test fixtures. */

type fixture;

/** [make_fixture setup tear_down] creates a test fixture running [setup]
before a test and [tear_down] after its complextion, successful or
not. */

let make_fixture: (unit => unit, unit => unit) => fixture;

/** A trivial test fixture, doing nothing. */

let relax: fixture;

/** A test fixture creating a temporary file before running the test and
deleting it after test completion.

[tmpfile prefix suffix r] creates a temporary file whose name is
derivated from the concatenation of [prefix], some appropriate
random number and a [suffix].  This name is stored in the
reference [r].

The temporary file is created empty, with permissions [0o600]
(readable and writable only by the file owner). The file is
guaranteed to be different from any other file that existed when
[tmpfile] was called.

@raise Sys_error if the file could not be created. */

let tmpfile: (string, string, ref(string)) => fixture;

/** A test fixture creating a temporary directory and making it the current
working directory before running the test and moving back to the previous
current working directory then deleting the temporary directory after test
completion. */

let tmpdir: (string, string, ref(string)) => fixture;

/** 6 Supervisors */

/** The abstract type of test supervisors. */

type supervisor;

/** A simple supervisor printing a lot of details on stdout and
    terminating with a summary on stderr. */

let verbose: supervisor;

/** A simple supervisor printing only details pertaining to test
    failures on stderr.

    A log file is prepared from the standard output of the given
    test suite. */

let concise: supervisor;

/** Used by test case code to signal its supervisor that a feature is
    not yet implemented. */

let not_implemented: unit => unit;

/** Used by test case code to signal its supervisor that a test should
    not be performed if the given condition is met. */

let skip_if: bool => unit;

/** Used by test case code to signal its supervisor that a test should
    only be performed when the given condition is met.

    This is equivalent to [skip_if] with a negated condition. */

let only_for: bool => unit;

/** 6 Test suites */

/** The abstract type of test suites. */

type suite;

/** [make_suite ident description] create an empty test suite, where test
    cases and test suites can be added with the infix operators. */

let make_suite: (~fixture: fixture=?, string, string) => suite;

/** Add a test case to a test suite. */

let add_case: (~fixture: fixture=?, suite, t) => unit;

/** Add a test suite to a test suite. */

let add_suite: (~fixture: fixture=?, suite, suite) => unit;

/** Register a test suite, so that it name can be exercised from the
    command line. */

let register: suite => unit;

/** [register_suite ident description lst] create and register a test
    suite, containing the test cases [lst]. */

let register_suite: (~fixture: fixture=?, string, string, list(t)) => unit;

/** Add a single test case to a test suite and return the suite. */

let (|&): (suite, t) => suite;

/** Add a list of cases to a test suite and return the suite. */

let (|@): (suite, list(t)) => suite;

/** An operator form of add_suite. */

let ( |* ): (suite, suite) => suite;

/** Add a list of suites to the test suite and return the suite. */

let (|:): (suite, list(suite)) => suite;

/** Main procedure for unitary tests.  It analyses the command line
and performs the appropriate actions, which typically involves running tests.

v
Usage: %s [-h | -l | suite1 [suite2 [...]]]
 Run unitary tests
Options:
 -h Display a cheerful help message.
 -l List available test suites.
 -x List all test cases marked as expected failures.
 -d Describe available test suites.
Exit Status:
 The %s program exits 0 on success and 1 if a test case failed.
v */

let main: unit => unit;
