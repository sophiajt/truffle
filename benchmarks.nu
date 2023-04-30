ls benchmarks | get name | each {|file|

  match ($file | path parse | get extension) {
    "lua" => { {name: luajit, time: (timeit luajit $file) } }
    "truffle" => { {name: truffle, time: (timeit ./target/release/truffle $file) } }
    "py" | "python" => { {name: python3, time: (timeit python3 $file) } }
  }
} | sort-by time