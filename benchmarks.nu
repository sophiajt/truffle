def benchmark [] {
  ls benchmarks | get name | each {|file|
    match ($file | path parse | get extension) {
      "lua" => { {name: luajit, time: (timeit luajit $file) } }
      "truffle" => { {name: truffle, time: (timeit ./target/release/truffle $file) } }
      "py" | "python" => { {name: pypy2, time: (timeit pypy $file) } }
      "js" => { {name: node, time: (timeit node $file) } }
    }
  } | sort-by time
}

print "warming up..."
benchmark | ignore
benchmark | ignore

print "benchmarking..."
benchmark