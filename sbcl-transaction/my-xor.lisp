(sb-c:defknown my-xor (fixnum fixnum) fixnum)

(sb-c:define-vop (my-xor)
  (:translate my-xor)
  (:args (a :scs (sb-vm::any-reg))
         (b :scs (sb-vm::any-reg)))
  (:arg-types fixnum fixnum)
  (:results (c :scs (sb-vm::any-reg)))
  (:result-types fixnum)
  (:policy :fast-safe)
  (:generator
   0
   (sb-c::inst sb-vm::mov c a)
   (sb-c::inst sb-vm::xor c b)))

(defun my-xor (a b)
  (my-xor a b))

(defun foo (a b)
  (declare (type fixnum a b)
           (optimize (speed 3) (safety 0)))
  (my-xor (1+ a) (1+ b)))

(disassemble 'my-xor)
