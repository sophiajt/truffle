def benchmark [] {
  ls benchmarks | get name | each {|file|
    match ($file | path parse | get extension) {
      "lua" => { {name: luajit, time: (timeit luajit $file) } }
      "truffle" => { {name: truffle, time: (timeit truffle $file) } }
      "pypy2" => { {name: pypy2, time: (timeit pypy $file) } }
      "py3" => { {name: python3, time: (timeit python3 $file) } }
      "js" => { {name: node, time: (timeit node $file) } }
      "jl" => { {name: julia, time: (timeit julia $file) } }
      "jl-jit" => { {name: julia-jit, time: (timeit julia $file) } }
      "rhai" => { {name: rhai, time: (timeit rhai_host $file) } }
    }
  } | sort-by time
}

print "warming up..."
benchmark | ignore
benchmark | ignore

print "benchmarking..."
benchmark
