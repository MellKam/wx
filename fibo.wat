(module
  (func $fibonacci (param $n i32) (result i32)
    ;; Base case: if n == 0, return 0
    (if (result i32)
      (i32.eq
        (local.get $n)
        (i32.const 0))
      (then
        (i32.const 0)) ;; Direct return
      (else
        ;; Base case: if n == 1, return 1
        (if (result i32)
          (i32.eq
            (local.get $n)
            (i32.const 1))
          (then
            (i32.const 1)) ;; Direct return
          (else
            ;; Recursive case: fibonacci(n-1) + fibonacci(n-2)
            (i32.add
              (call $fibonacci
                (i32.sub
                  (local.get $n)
                  (i32.const 1)))
              (call $fibonacci
                (i32.sub
                  (local.get $n)
                  (i32.const 2)))))))))

  (export "fibonacci" (func $fibonacci)))
