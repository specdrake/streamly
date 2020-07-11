#!/bin/bash

#------------------------------------------------------------------------------
# Benchmark groups
#------------------------------------------------------------------------------

# IMPORTANT NOTE: the names "_grp" and "_cmp" suffixes are special, do
# not rename them to something else.

base_stream_grp="\
    Data.Stream.StreamD \
    Data.Stream.StreamK \
    Data.Stream.StreamDK"

prelude_serial_grp="\
  Prelude.Serial \
  Prelude.WSerial \
  Prelude.ZipSerial"

prelude_concurrent_grp="\
  Prelude.Async \
  Prelude.WAsync \
  Prelude.Ahead \
  Prelude.Parallel \
  Prelude.ZipAsync"

prelude_other_grp="\
  Prelude.Rate \
  Prelude.Concurrent \
  Prelude.Adaptive"

array_grp="Memory.Array Data.Array Data.Prim.Array Data.SmallArray"

base_parser_grp="Data.Parser.ParserD Data.Parser.ParserK"
parser_grp="Data.Fold Data.Parser"

#------------------------------------------------------------------------------
# Streaming vs non-streaming
#------------------------------------------------------------------------------
# The "o-1-space" groups of these benchmarks are run with long stream
# sizes when --long option is used.

infinite_grp="\
  $prelude_serial_grp \
  $prelude_concurrent_grp \
  Prelude.Rate"

#------------------------------------------------------------------------------
# Comparison groups
#------------------------------------------------------------------------------
# *_cmp denotes a comparison benchmarks, the benchmarks provided in *_cmp
# variables are compared with each other
base_stream_cmp="Data.Stream.StreamD Data.Stream.StreamK"
serial_wserial_cmp="Prelude.Serial Prelude.WSerial"
serial_async_cmp="Prelude.Serial Prelude.Async"
concurrent_cmp="Prelude.Async Prelude.WAsync Prelude.Ahead Prelude.Parallel"
array_cmp="Memory.Array Data.Prim.Array Data.Array"
base_parser_cmp=$base_parser_grp
COMPARISONS="\
  base_stream_cmp \
  serial_wserial_cmp \
  serial_async_cmp \
  concurrent_cmp \
  array_cmp \
  base_parser_cmp"

#------------------------------------------------------------------------------
# All
#------------------------------------------------------------------------------
# All high level benchmarks
all_grp="\
    $prelude_serial_grp \
    $prelude_concurrent_grp \
    $array_grp \
    $parser_grp \
    Data.Unfold \
    FileSystem.Handle"

ALL_BENCH_GROUPS="\
    all_grp \
    prelude_serial_grp \
    prelude_concurrent_grp \
    array_grp \
    infinite_grp \
    base_stream_grp \
    base_parser_grp"

#------------------------------------------------------------------------------
# Script
#------------------------------------------------------------------------------

BENCH_SH_DIR=$(dirname $0)

list_benches ()  {
  echo "Individual benchmarks:"
  for i in $all_grp
  do
    echo "$i"
  done
  echo
}

list_bench_groups ()  {
  echo "Benchmark groups:"
  for i in $ALL_BENCH_GROUPS
  do
    echo -n "$i ["
    eval "echo -n \$$i"
    echo "]"
  done
  echo
}

list_comparisons ()  {
  echo "Comparison groups:"
  for i in $COMPARISONS
  do
    echo -n "$i ["
    eval "echo -n \$$i"
    echo "]"
  done
  echo
}

print_help () {
  echo "Usage: $0 "
  echo "       [--benchmarks <"bench1 bench2 ..." | help>]"
  echo "       [--fields <"field1 field2 ..." | help>]"
  echo "       [--graphs]"
  echo "       [--no-measure]"
  echo "       [--append]"
  echo "       [--long]"
  echo "       [--slow]"
  echo "       [--quick]"
  echo "       [--cabal-build-flags <flag>]"
  echo "       [--with-chart-exe <path>]"
  echo "       [--compare] [--base <commit>] [--candidate <commit>]"
  echo "       -- <gauge options or benchmarks>"
  echo
  echo "--benchmarks: benchmarks to run, use 'help' for list of benchmarks"
  echo "--fields: measurement fields to report, use 'help' for a list"
  echo "--graphs: Generate graphical reports"
  echo "--no-measure: Don't run benchmarks, run reports from previous results"
  echo "--append: Don't overwrite previous results, append for comparison"
  echo "--long: Use much longer stream size for infinite stream benchmarks"
  echo "--slow: Slightly more accurate results at the expense of speed"
  echo "--quick: Faster results, useful for longer benchmarks"
  echo "--with-chart-exe: Provide a custom path for the chart executable"
  echo "--cabal-build-flags: Pass any cabal builds flags to be used for build"
  echo
  echo "When specific space complexity group is chosen then (and only then) "
  echo "RTS memory restrictions are used accordingly. For example, "
  echo "bench.sh --benchmarks Data.Parser -- Data.Parser/o-1-space "
  echo "restricts Heap/Stack space for O(1) characterstics"
  echo
  echo "When using --compare, by default comparative chart of HEAD^ vs HEAD"
  echo "commit is generated, in the 'charts' directory."
  echo "Use --base and --candidate to select the commits to compare."
  echo
  echo "Any arguments after a '--' are passed directly to gauge"
  exit
}

# $1: message
die () {
  >&2 echo -e "Error: $1"
  exit 1
}

set_benchmarks() {
  if test -z "$BENCHMARKS"
  then
    echo $DEFAULT_BENCHMARKS
  else
    for i in $(echo $BENCHMARKS)
    do
        case $i in
          *_grp) eval "echo -n \$${i}" ;;
          *_cmp) eval "echo -n \$${i} $i" ;;
          *) echo -n $i ;;
        esac
        echo -n " "
    done
  fi
}

find_report_prog() {
    local prog_name="chart"
    hash -r # XXX Why is this required?
    if test -z "$CHART_EXE"
    then
      CHART_EXE=$(which $prog_name)
    fi
    if test -x "$CHART_EXE"
    then
      echo $CHART_EXE
    else
      return 1
    fi
}

# We run the benchmarks in isolation in a separate process so that different
# benchmarks do not interfere with other. To enable that we need to pass the
# benchmark exe path to gauge as an argument. Unfortunately it cannot find its
# own path currently.

# The path is dependent on the architecture and cabal version.
# Use this command to find the exe if this script fails with an error:
# find .stack-work/ -type f -name "benchmarks"
# $1: benchmark name (Prelude.Serial, Prelude.WSerial)
stack_bench_prog () {
  local bench_name=$1
  local bench_prog=`stack path --dist-dir`/build/$bench_name/$bench_name
  if test -x "$bench_prog"
  then
    echo $bench_prog
  else
    return 1
  fi
}

# $1: benchmark name (Prelude.Serial, Prelude.WSerial)
cabal_bench_prog () {
  local bench_name=$1
  local bench_prog=`$WHICH_COMMAND $1`
  if test -x "$bench_prog"
  then
    echo $bench_prog
  else
    return 1
  fi
}

# $1: benchmark name (Prelude.Serial, Prelude.WSerial)
bench_output_file() {
    local bench_name=$1
    echo "charts/$bench_name/results.csv"
}

# $1: command
function run_verbose() {
  echo "$*"
  bash -c "$*"
}

# --min-duration 0 means exactly one iteration per sample. We use a million
# iterations in the benchmarking code explicitly and do not use the iterations
# done by the benchmarking tool.
#
# Benchmarking tool by default discards the first iteration to remove
# aberrations due to initial evaluations etc. We do not discard it because we
# are anyway doing iterations in the benchmarking code and many of them so that
# any constant factor gets amortized and anyway it is a cost that we pay in
# real life.
#
# We can pass --min-samples value from the command line as second argument
# after the benchmark name in case we want to use more than one sample.
# $1: benchmark name (Prelude.Serial, Prelude.WSerial)
run_bench () {
  local bench_name=$1
  local bench_exe=$bench_name
  local output_file=$(bench_output_file $bench_name)
  local bench_prog
  bench_prog=$($GET_BENCH_PROG $bench_exe) || \
    die "Cannot find benchmark executable for benchmark $bench_name"

  mkdir -p `dirname $output_file`

  echo "Running benchmark $bench_name ..."

  local QUICK_OPTS="--quick --min-duration 0"
  local SPEED_OPTIONS
  if test "$LONG" -eq 0
  then
    if test "$SLOW" -eq 0
    then
        export QUICK_MODE
        if test "$QUICK_MODE" -eq 0
        then
          # default mode, not super quick, not slow
          SPEED_OPTIONS="$QUICK_OPTS --min-samples 10 --time-limit 1"
        else
          # super quick but less accurate
          SPEED_OPTIONS="$QUICK_OPTS --time-limit 0 --include-first-iter"
        fi
    else
      # Slow but more accurate mode
      SPEED_OPTIONS="--min-duration 0"
    fi
  else
      # large stream size, always super quick
      GAUGE_ARGS="$GAUGE_ARGS $bench_name/o-1-space"
      SPEED_OPTIONS="--stream-size 10000000 $QUICK_OPTS --include-first-iter"
  fi

  export BENCH_EXEC_PATH=$bench_prog
  export RTS_OPTIONS
  run_verbose $bench_prog $SPEED_OPTIONS \
    --csvraw=$output_file \
    -v 2 \
    --measure-with "$BENCH_SH_DIR/bin/bench-exec-one.sh" \
    $GAUGE_ARGS || die "Benchmarking failed"
}

# $1: multiple benchmark names "Prelude.Serial Prelude.Rate"
run_benches() {
    for i in $1
    do
      run_bench $i
    done
}

# $1: multiple benchmark names "Prelude.Serial Prelude.Rate"
run_benches_comparing() {
    local bench_list=$1

    if test -z "$CANDIDATE"
    then
      CANDIDATE=$(git rev-parse HEAD)
    fi
    if test -z "$BASE"
    then
      # XXX Should be where the current branch is forked from master
      BASE="$CANDIDATE^"
    fi
    echo "Comparing baseline commit [$BASE] with candidate [$CANDIDATE]"
    echo "Checking out base commit [$BASE] for benchmarking"
    git checkout "$BASE" || die "Checkout of base commit [$BASE] failed"

    $BUILD_BENCH || die "build failed"
    run_benches "$bench_list"

    echo "Checking out candidate commit [$CANDIDATE] for benchmarking"
    git checkout "$CANDIDATE" || \
        die "Checkout of candidate [$CANDIDATE] commit failed"

    $BUILD_BENCH || die "build failed"
    run_benches "$bench_list"
    # XXX reset back to the original commit
}

# $1: benchmark name (Prelude.Serial, Prelude.WSerial)
backup_output_file() {
  local bench_name=$1
  local output_file=$(bench_output_file $bench_name)

  if test -e $output_file -a "$APPEND" != 1
  then
      mv -f -v $output_file ${output_file}.prev
  fi
}

# $1: multiple benchmark names "Prelude.Serial Prelude.Rate"
run_measurements() {
  local bench_list=$1

  for i in $bench_list
  do
      backup_output_file $i
  done

  if test "$COMPARE" = "0"
  then
    run_benches "$bench_list"
  else
    run_benches_comparing "$bench_list"
  fi
}

run_reports() {
    local prog
    prog=$(find_report_prog) || \
      die "Cannot find bench-graph executable \n\
You can install it via \"cabal install chart -f dev\" \
or provide the path of the executable using --with-chart-exe"
    echo

    for i in $1
    do
        echo "Generating reports for ${i}..."
        $prog $(test "$GRAPH" = 1 && echo "--graphs") \
              --benchmark $i --fields "$FIELDS"
    done
}

#-----------------------------------------------------------------------------
# Execution starts here
#-----------------------------------------------------------------------------

DEFAULT_BENCHMARKS="$all_grp"
DEFAULT_FIELDS="allocated bytescopied cputime"
ALL_FIELDS="$FIELDS time cycles utime stime minflt majflt nvcsw nivcsw"
FIELDS=$DEFAULT_FIELDS

COMPARE=0
BASE=
CANDIDATE=

APPEND=0
SLOW=0
QUICK_MODE=0
LONG=0
RAW=0
GRAPH=0
MEASURE=1

GAUGE_ARGS=
RTS_OPTIONS=
BUILD_ONCE=0
USE_STACK=0
CABAL_BUILD_FLAGS=""

CHART_EXE=""

GHC_VERSION=$(ghc --numeric-version)

CABAL_BUILD_DIR="dist-newstyle"

cabal_which() {
  if test -z "$CABAL_BUILD_DIR"
  then
    CABAL_BUILD_DIR="dist-newstyle"
  fi
  find "$CABAL_BUILD_DIR/build" -type f -path "*${GHC_VERSION}*/$1"
}

#-----------------------------------------------------------------------------
# Read command line
#-----------------------------------------------------------------------------

while test -n "$1"
do
  case $1 in
    -h|--help|help) print_help ;;
    # options with arguments
    --benchmarks) shift; BENCHMARKS=$1; shift ;;
    --fields) shift; FIELDS=$1; shift ;;
    --base) shift; BASE=$1; shift ;;
    --candidate) shift; CANDIDATE=$1; shift ;;
    --cabal-build-flags) shift; CABAL_BUILD_FLAGS=$1; shift ;;
    --rtsopts) shift; RTS_OPTIONS=$1; shift ;;
    # flags
    --slow) SLOW=1; shift ;;
    --quick) QUICK_MODE=1; shift ;;
    --compare) COMPARE=1; shift ;;
    --raw) RAW=1; shift ;;
    --append) APPEND=1; shift ;;
    --long) LONG=1; shift ;;
    --graphs) GRAPH=1; shift ;;
    --no-measure) MEASURE=0; shift ;;
    --with-chart-exe) CHART_EXE=$1; shift ;;
    --) shift; break ;;
    -*|--*) print_help ;;
    *) break ;;
  esac
done
GAUGE_ARGS=$*

# Parse cabal build flags for any quirks
# Dont specify the "build" directory of another build.
# Legal: dist-newstyle, dist-newstyle/commit-hash
# Illegal: dist-newstyle/build
for option in "$CABAL_BUILD_FLAGS"
do
  case $option in
    --builddir=*) CABAL_BUILD_DIR=${option:11}; break ;;
  esac
done

only_real_benchmarks () {
  for i in $BENCHMARKS
  do
    local SKIP=0
    for j in $COMPARISONS
    do
      if test $i == $j
      then
        SKIP=1
      fi
    done
    if test "$SKIP" -eq 0
    then
      echo -n "$i "
    fi
  done
}

# $1: multiple names "apple mango"
# $2: single name "mango"
has_item () {
  for i in $1
  do
    if test "$i" = "$2"
    then
      echo "$i"
      break
    fi
  done
}

BENCHMARKS_ORIG=$BENCHMARKS
if test "$(has_item "$BENCHMARKS_ORIG" help)" = "help"
then
  list_bench_groups
  list_comparisons
  list_benches
  exit
fi

if test "$(has_item "$FIELDS" help)" = "help"
then
  echo "Supported fields: $ALL_FIELDS"
  echo "Default fields: $DEFAULT_FIELDS"
  exit
fi

if test "$LONG" -ne 0
then
  if test -n "$BENCHMARKS"
  then
    echo "Cannot specify benchmarks [$BENCHMARKS] with --long"
    exit
  fi
  BENCHMARKS=$infinite_grp
fi

BENCHMARKS=$(set_benchmarks)
BENCHMARKS_ORIG=$BENCHMARKS
BENCHMARKS=$(only_real_benchmarks)
EXECUTABLES=$BENCHMARKS

echo "Using benchmark suites [$BENCHMARKS]"

if test "$USE_STACK" = "1"
then
  WHICH_COMMAND="stack exec which"
  GET_BENCH_PROG=stack_bench_prog
  BUILD_BENCH="stack build $STACK_BUILD_FLAGS --bench --no-run-benchmarks"
else
  # XXX cabal issue "cabal v2-exec which" cannot find benchmark/test executables
  #WHICH_COMMAND="cabal v2-exec which"
  WHICH_COMMAND=cabal_which
  GET_BENCH_PROG=cabal_bench_prog
  BUILD_BENCH="cabal v2-build $CABAL_BUILD_FLAGS --enable-benchmarks"
fi

#-----------------------------------------------------------------------------
# Run benchmarks
#-----------------------------------------------------------------------------

if test "$MEASURE" = "1"
then
  echo $BUILD_BENCH
  if test "$USE_STACK" = "1"
  then
    $BUILD_BENCH || die "build failed"
  else
    $BUILD_BENCH $EXECUTABLES || die "build failed"
  fi
  run_measurements "$BENCHMARKS"
fi

#-----------------------------------------------------------------------------
# Run reports
#-----------------------------------------------------------------------------

COMPARISON_REPORTS=""
for i in $COMPARISONS
do
  if test "$(has_item "$BENCHMARKS_ORIG" $i)" = $i
  then
    COMPARISON_REPORTS="$COMPARISON_REPORTS $i"
    mkdir -p "charts/$i"
    constituents=$(eval "echo -n \$${i}")
    dest_file="charts/$i/results.csv"
    : > $dest_file
    for j in $constituents
    do
      cat "charts/$j/results.csv" >> $dest_file
    done
  fi
done

if test "$RAW" = "0"
then
  run_reports "$BENCHMARKS"
  run_reports "$COMPARISON_REPORTS"
fi
