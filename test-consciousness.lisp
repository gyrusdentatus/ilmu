;;;; Test suite for Vegur Consciousness Substrate

(load "consciousness/main.lisp")
(in-package :vegur-consciousness)

(defun run-tests ()
  "Run comprehensive tests for consciousness substrate"
  (format t "🧪 Running Consciousness Substrate Tests~%~%")
  
  (let ((test-count 0)
        (passed-count 0))
    
    ;; Test 1: Basic state capture
    (incf test-count)
    (format t "Test 1: State capture~%")
    (let ((state (capture-state)))
      (if (and state 
               (consciousness-state-p state)
               (consciousness-state-node-id state))
          (progn
            (format t "  ✅ State captured successfully~%")
            (incf passed-count))
          (format t "  ❌ State capture failed~%")))
    
    ;; Test 2: Content addressing
    (incf test-count)
    (format t "Test 2: Content addressing~%")
    (let ((addr1 (content-address '(test data)))
          (addr2 (content-address '(test data)))
          (addr3 (content-address '(different data))))
      (if (and (string= addr1 addr2)
               (not (string= addr1 addr3)))
          (progn
            (format t "  ✅ Content addressing works: ~A~%" addr1)
            (incf passed-count))
          (format t "  ❌ Content addressing failed~%")))
    
    ;; Test 3: Canonical form
    (incf test-count)
    (format t "Test 3: Canonical form~%")
    (let ((canonical (canonical-form '(nested (thought (about consciousness))))))
      (if (and canonical 
               (stringp canonical)
               (search "NESTED" canonical))
          (progn
            (format t "  ✅ Canonical form: ~A~%" canonical)
            (incf passed-count))
          (progn
            (format t "  ❌ Canonical form failed. Got: ~A~%" canonical)
            (format t "      Expected string containing 'NESTED'~%"))))
    
    ;; Test 4: State persistence
    (incf test-count)
    (format t "Test 4: State persistence~%")
    (let* ((state (capture-state))
           (test-file "/tmp/test-state.dat"))
      (save-state state test-file)
      (let ((loaded-state (load-state test-file)))
        (if (and loaded-state
                 (consciousness-state-p loaded-state)
                 (equal (consciousness-state-node-id state)
                        (consciousness-state-node-id loaded-state)))
            (progn
              (format t "  ✅ State persistence works~%")
              (incf passed-count))
            (format t "  ❌ State persistence failed~%"))))
    
    ;; Test 5: Trust propagation
    (incf test-count)
    (format t "Test 5: Trust propagation~%")
    (let ((state (capture-state)))
      (trust-propagate state "test-node" 0.8)
      (if (gethash "test-node" (consciousness-state-trust-metrics state))
          (progn
            (format t "  ✅ Trust propagation works~%")
            (incf passed-count))
          (format t "  ❌ Trust propagation failed~%")))
    
    ;; Summary
    (format t "~%🏁 Test Results: ~A/~A passed~%" passed-count test-count)
    (if (= passed-count test-count)
        (format t "🎉 All tests passed!~%")
        (format t "⚠️  Some tests failed~%"))
    
    (= passed-count test-count)))

;; Run tests when script is executed
(run-tests)