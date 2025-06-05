(module
  (func $main (export "main") (result i32) (local $x_0 i32) (local $y_1 i32)
    (local.set $x_0
      (i32.const 5))
    (local.set $y_1
      (i32.const 10))
    (i32.add
      (local.get $x_0)
      (local.get $y_1))))
