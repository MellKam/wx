(module
  (func $example_if (param $i i32) (result i32)
    (local.get $i)
    (if $my_if (result i32)
      (then
        (local.get $i)
        (i32.const 5)
        (i32.eq)
        (br_if $my_if)
        (i32.const 100))
      (else
        (i32.const -1)))))
