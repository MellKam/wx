(module
  (func (export "run") (result i32) (local $a i32)
    (local.set $a (
      i32.add (local.get $a) (i32.const 5)
    ))
    (local.get $a)
  )
)