ls benchmarks | get name | each {|file|

  match ($file | path parse | get extension) {
    "lua" => { {name: luajit, time: (timeit luajit $file) } }
    "truffle" => { {name: truffle, time: (timeit ./target/release/truffle $file) } }
    "py" | "python" => { {name: python3, time: (timeit python3 $file) } }
    "js" => { {name: node, time: (timeit node $file) } }
  }
} | sort-by time

    # "py" | "python" => { {name: pypy, time: (timeit pypy $file) } }
