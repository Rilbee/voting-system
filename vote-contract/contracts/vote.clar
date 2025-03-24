;; Voting Contract
;; This contract implements a secure and flexible voting system on the Stacks blockchain


;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-VOTED (err u101))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u102))
(define-constant ERR-VOTING-CLOSED (err u103))
(define-constant ERR-VOTING-NOT-CLOSED (err u104))
(define-constant ERR-INVALID-VOTE-OPTION (err u105))
(define-constant ERR-UNAUTHORIZED-CALLER (err u106))
(define-constant ERR-MINIMUM-VOTES-NOT-MET (err u107))
(define-constant ERR-EMPTY-OPTIONS (err u108))
(define-constant ERR-INVALID-WEIGHT (err u109))

;; Data structures
(define-map proposals 
  { proposal-id: uint }
  {
    title: (string-utf8 256),
    description: (string-utf8 1024),
    creator: principal,
    start-block-height: uint,
    end-block-height: uint,
    options: (list 10 (string-utf8 64)),
    is-active: bool,
    minimum-votes: uint
  }
)

(define-map votes
  { proposal-id: uint, voter: principal }
  { option-idx: uint }
)

(define-map vote-counts
  { proposal-id: uint, option-idx: uint }
  { count: uint }
)

(define-map voter-weights
  { voter: principal }
  { weight: uint }
)

(define-map proposal-results
  { proposal-id: uint }
  {
    winning-option-idx: uint,
    winning-option-count: uint,
    total-votes: uint,
    is-finalized: bool
  }
)

;; Variables
(define-data-var proposal-count uint u0)
(define-data-var contract-owner principal tx-sender)

;; Read-only functions

(define-read-only (get-proposal-count)
  (var-get proposal-count)
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-vote-count (proposal-id uint) (option-idx uint))
  (default-to { count: u0 } 
    (map-get? vote-counts { proposal-id: proposal-id, option-idx: option-idx }))
)

(define-read-only (get-voter-weight (voter principal))
  (default-to { weight: u1 } 
    (map-get? voter-weights { voter: voter }))
)

(define-read-only (get-proposal-result (proposal-id uint))
  (map-get? proposal-results { proposal-id: proposal-id })
)

(define-read-only (has-voted (proposal-id uint) (voter principal))
  (is-some (map-get? votes { proposal-id: proposal-id, voter: voter }))
)

(define-read-only (is-voting-open (proposal-id uint))
  (match (map-get? proposals { proposal-id: proposal-id })
    proposal (and
              (get is-active proposal)
              (>= block-height (get start-block-height proposal))
              (<= block-height (get end-block-height proposal)))
    false
  )
)

;; Create a new proposal
(define-public (create-proposal 
                (title (string-utf8 256)) 
                (description (string-utf8 1024)) 
                (options (list 10 (string-utf8 64)))
                (duration uint)
                (minimum-votes uint))
  (let ((proposal-id (var-get proposal-count))
        (start-block (+ block-height u1))
        (end-block (+ block-height duration))
        (option-count (len options)))
    
    ;; Verify options list is not empty
    (asserts! (> option-count u0) ERR-EMPTY-OPTIONS)
    
    ;; Store the proposal details
    (map-set proposals
      { proposal-id: proposal-id }
      {
        title: title,
        description: description,
        creator: tx-sender,
        start-block-height: start-block,
        end-block-height: end-block,
        options: options,
        is-active: true,
        minimum-votes: minimum-votes
      }
    )
    
    ;; Initialize vote counts
    (init-option-0 proposal-id)
    (init-option-1 proposal-id)
    (init-option-2 proposal-id)
    (init-option-3 proposal-id)
    (init-option-4 proposal-id)
    (init-option-5 proposal-id)
    (init-option-6 proposal-id)
    (init-option-7 proposal-id)
    (init-option-8 proposal-id)
    (init-option-9 proposal-id)
    
    ;; Increment the proposal counter
    (var-set proposal-count (+ proposal-id u1))
    
    ;; Return success with the new proposal ID
    (ok proposal-id)
  )
)

;; Individual option initializers to avoid circular dependencies
(define-private (init-option-0 (proposal-id uint))
  (map-set vote-counts
    { proposal-id: proposal-id, option-idx: u0 }
    { count: u0 }
  )
)

(define-private (init-option-1 (proposal-id uint))
  (map-set vote-counts
    { proposal-id: proposal-id, option-idx: u1 }
    { count: u0 }
  )
)

(define-private (init-option-2 (proposal-id uint))
  (map-set vote-counts
    { proposal-id: proposal-id, option-idx: u2 }
    { count: u0 }
  )
)

(define-private (init-option-3 (proposal-id uint))
  (map-set vote-counts
    { proposal-id: proposal-id, option-idx: u3 }
    { count: u0 }
  )
)

(define-private (init-option-4 (proposal-id uint))
  (map-set vote-counts
    { proposal-id: proposal-id, option-idx: u4 }
    { count: u0 }
  )
)

(define-private (init-option-5 (proposal-id uint))
  (map-set vote-counts
    { proposal-id: proposal-id, option-idx: u5 }
    { count: u0 }
  )
)

(define-private (init-option-6 (proposal-id uint))
  (map-set vote-counts
    { proposal-id: proposal-id, option-idx: u6 }
    { count: u0 }
  )
)

(define-private (init-option-7 (proposal-id uint))
  (map-set vote-counts
    { proposal-id: proposal-id, option-idx: u7 }
    { count: u0 }
  )
)

(define-private (init-option-8 (proposal-id uint))
  (map-set vote-counts
    { proposal-id: proposal-id, option-idx: u8 }
    { count: u0 }
  )
)

(define-private (init-option-9 (proposal-id uint))
  (map-set vote-counts
    { proposal-id: proposal-id, option-idx: u9 }
    { count: u0 }
  )
)

;; Cast a vote for a proposal
(define-public (vote (proposal-id uint) (option-idx uint))
  (let ((voter tx-sender)
        (weight (get weight (get-voter-weight voter))))
    
    ;; Check if the proposal exists
    (asserts! (is-some (map-get? proposals { proposal-id: proposal-id }))
              ERR-PROPOSAL-NOT-FOUND)
    
    (let ((proposal (unwrap-panic (map-get? proposals { proposal-id: proposal-id }))))
      ;; Check if voting is open
      (asserts! (is-voting-open proposal-id) ERR-VOTING-CLOSED)
      
      ;; Check if the option is valid
      (asserts! (< option-idx (len (get options proposal))) ERR-INVALID-VOTE-OPTION)
      
      ;; Check if user has already voted
      (asserts! (not (has-voted proposal-id voter)) ERR-ALREADY-VOTED)
      
      ;; Record the vote
      (map-set votes
        { proposal-id: proposal-id, voter: voter }
        { option-idx: option-idx }
      )
      
      ;; Update the vote count
      (let ((current-count (get count (get-vote-count proposal-id option-idx))))
        (map-set vote-counts
          { proposal-id: proposal-id, option-idx: option-idx }
          { count: (+ current-count weight) }
        )
      )
      
      (ok true)
    )
  )
)

;; Finalize and compute the results of a proposal
(define-public (finalize-proposal (proposal-id uint))
  (let ((proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND)))
    ;; Check if voting period has ended
    (asserts! (> block-height (get end-block-height proposal)) ERR-VOTING-NOT-CLOSED)
    
    ;; Check if proposal is still active
    (asserts! (get is-active proposal) ERR-VOTING-CLOSED)
    
    ;; Calculate results - explicitly check each option
    (let (
        (count-0 (get count (get-vote-count proposal-id u0)))
        (count-1 (get count (get-vote-count proposal-id u1)))
        (count-2 (get count (get-vote-count proposal-id u2)))
        (count-3 (get count (get-vote-count proposal-id u3)))
        (count-4 (get count (get-vote-count proposal-id u4)))
        (count-5 (get count (get-vote-count proposal-id u5)))
        (count-6 (get count (get-vote-count proposal-id u6)))
        (count-7 (get count (get-vote-count proposal-id u7)))
        (count-8 (get count (get-vote-count proposal-id u8)))
        (count-9 (get count (get-vote-count proposal-id u9)))
        (option-count (len (get options proposal)))
      )
      
      ;; Calculate total votes
      (let (
          (total-votes (+ count-0 (+ count-1 (+ count-2 (+ count-3 (+ count-4 (+ count-5 (+ count-6 (+ count-7 (+ count-8 count-9))))))))))
          
          ;; Find winning option - explicit comparison to avoid recursion
          (best-count count-0)
          (best-idx u0)
          
          ;; Compare each option against current best
          (best-count-1 (if (> count-1 best-count) count-1 best-count))
          (best-idx-1 (if (> count-1 best-count) u1 best-idx))
          
          (best-count-2 (if (> count-2 best-count-1) count-2 best-count-1))
          (best-idx-2 (if (> count-2 best-count-1) u2 best-idx-1))
          
          (best-count-3 (if (> count-3 best-count-2) count-3 best-count-2))
          (best-idx-3 (if (> count-3 best-count-2) u3 best-idx-2))
          
          (best-count-4 (if (> count-4 best-count-3) count-4 best-count-3))
          (best-idx-4 (if (> count-4 best-count-3) u4 best-idx-3))
          
          (best-count-5 (if (> count-5 best-count-4) count-5 best-count-4))
          (best-idx-5 (if (> count-5 best-count-4) u5 best-idx-4))
          
          (best-count-6 (if (> count-6 best-count-5) count-6 best-count-5))
          (best-idx-6 (if (> count-6 best-count-5) u6 best-idx-5))
          
          (best-count-7 (if (> count-7 best-count-6) count-7 best-count-6))
          (best-idx-7 (if (> count-7 best-count-6) u7 best-idx-6))
          
          (best-count-8 (if (> count-8 best-count-7) count-8 best-count-7))
          (best-idx-8 (if (> count-8 best-count-7) u8 best-idx-7))
          
          (best-count-9 (if (> count-9 best-count-8) count-9 best-count-8))
          (best-idx-9 (if (> count-9 best-count-8) u9 best-idx-8))
        )
        
        ;; Check minimum votes threshold
        (asserts! (>= total-votes (get minimum-votes proposal)) ERR-MINIMUM-VOTES-NOT-MET)
        
        ;; Set proposal as inactive
        (map-set proposals
          { proposal-id: proposal-id }
          (merge proposal { is-active: false })
        )
        
        ;; Store the results
        (map-set proposal-results
          { proposal-id: proposal-id }
          {
            winning-option-idx: best-idx-9,
            winning-option-count: best-count-9,
            total-votes: total-votes,
            is-finalized: true
          }
        )
        
        (ok best-idx-9)
      )
    )
  )
)

;; Change a voter's weight (only callable by contract owner)
(define-public (set-voter-weight (voter principal) (weight uint))
  (begin
    ;; Only contract owner can set weights
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-UNAUTHORIZED-CALLER)
    
    ;; Weight must be at least 1
    (asserts! (> weight u0) ERR-INVALID-WEIGHT)
    
    ;; Set the weight
    (map-set voter-weights
      { voter: voter }
      { weight: weight }
    )
    
    (ok true)
  )
)

;; Cancel a proposal (only callable by the proposal creator or contract owner)
(define-public (cancel-proposal (proposal-id uint))
  (let ((proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND)))
    ;; Check authorization (creator or contract owner)
    (asserts! (or 
              (is-eq tx-sender (get creator proposal))
              (is-eq tx-sender (var-get contract-owner)))
            ERR-NOT-AUTHORIZED)
    
    ;; Set as inactive
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal { is-active: false })
    )
    
    (ok true)
  )
)

;; Transfer contract ownership
(define-public (transfer-ownership (new-owner principal))
  (begin
    ;; Only current owner can transfer ownership
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    
    ;; Set new owner
    (var-set contract-owner new-owner)
    
    (ok true)
  )
)