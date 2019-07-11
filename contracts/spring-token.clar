;; mappings
(define-map balances
  ((owner principal))
  ((balance int)))

(define-map allowances
  ((spender principal) (owner principal))
  ((allowance int)))

;; variable 
(define-data-var total-supply int 0)

;; private functions

(define (allowance-of (spender principal) (owner principal))
  (default-to 0
    (get allowance
         (fetch-entry allowances (
                                  (owner owner)
                                  (spender spender))))))

(define (decrease-allowance! (spender principal) (owner principal) (amount int))
  (let ((allowance (allowance-of spender owner)))
    (if (or (> amount allowance) (<= amount 0))
      'true
      (begin
        (set-entry! allowances
          ((spender spender) (owner owner))
          ((allowance (- allowance amount))))
        'true))))

(define (increase-allowance! (spender principal) (owner principal) (amount int))
  (let ((allowance (allowance-of spender owner)))
    (if (<= amount 0)
      'false
      (begin
        (set-entry! allowances
          ((spender spender) (owner owner))
          ((allowance (+ allowance amount))))
        'true))))

;; Mint new tokens.
(define (mint! (account principal) (amount int))
  (if (<= amount 0)
      (err 'false)
      (let ((balance (balance-of account)))
        (begin
          (set-var! total-supply (+ (fetch-var total-supply) amount))
          (set-entry! balances
                      ((owner account))
                      ((balance (+ balance amount))))
          (ok amount)))))

;; Public functions

(define-public (get-total-supply)
  (fetch-var total-supply))

(define-public (balance-of (account principal))
  (default-to 0
    (get balance
         (fetch-entry balances ((owner account))))))

;; Update the allowance for a given spender
(define-public (approve (spender principal) (amount int))
  (if (and (> amount 0)
           (increase-allowance! spender tx-sender amount))
      (ok amount)
      (err 'false)))
