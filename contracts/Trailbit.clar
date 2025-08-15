(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-invalid-amount (err u104))
(define-constant err-unauthorized (err u105))
(define-constant err-trail-inactive (err u106))
(define-constant err-invalid-rating (err u107))
(define-constant err-already-rated (err u108))
(define-constant err-review-too-long (err u109))
(define-constant err-cannot-rate-own-trail (err u110))
(define-constant err-invalid-condition-type (err u111))
(define-constant err-condition-already-reported (err u112))
(define-constant err-condition-expired (err u113))
(define-constant err-cannot-verify-own-report (err u114))
(define-constant err-condition-not-found (err u115))

(define-fungible-token trailbit)

(define-data-var token-name (string-ascii 32) "Trailbit")
(define-data-var token-symbol (string-ascii 10) "TRAIL")
(define-data-var token-decimals uint u6)
(define-data-var total-supply uint u0)

(define-map trails
  { trail-id: uint }
  {
    name: (string-ascii 64),
    location: (string-ascii 128),
    difficulty: uint,
    creator: principal,
    reward-pool: uint,
    discovery-reward: uint,
    maintenance-reward: uint,
    created-at: uint,
    active: bool,
    total-visits: uint,
    total-ratings: uint,
    average-rating: uint,
    rating-sum: uint
  }
)

(define-map trail-visits
  { trail-id: uint, visitor: principal }
  {
    visit-count: uint,
    last-visit: uint,
    total-earned: uint
  }
)

(define-map trail-maintenance
  { trail-id: uint, maintainer: principal }
  {
    maintenance-count: uint,
    last-maintenance: uint,
    total-earned: uint
  }
)

(define-map user-stats
  { user: principal }
  {
    trails-created: uint,
    trails-visited: uint,
    maintenance-performed: uint,
    total-earned: uint
  }
)

(define-map trail-ratings
  { trail-id: uint, rater: principal }
  {
    rating: uint,
    review: (string-ascii 256),
    timestamp: uint,
    helpful-votes: uint
  }
)

(define-map trail-reviews
  { trail-id: uint }
  {
    review-count: uint,
    featured-review: (optional (string-ascii 256)),
    featured-reviewer: (optional principal)
  }
)

(define-map user-review-stats
  { user: principal }
  {
    reviews-given: uint,
    helpful-votes-received: uint,
    reviewer-level: uint
  }
)

(define-map review-helpfulness
  { trail-id: uint, reviewer: principal, voter: principal }
  {
    voted: bool,
    vote-type: bool
  }
)

(define-data-var next-trail-id uint u1)
(define-data-var platform-fee uint u50)
(define-data-var min-rating uint u1)
(define-data-var max-rating uint u5)
(define-data-var quality-threshold uint u4)
(define-data-var bonus-multiplier uint u150)

;; Trail condition reporting constants
(define-constant condition-clear u1)
(define-constant condition-muddy u2)
(define-constant condition-icy u3)
(define-constant condition-blocked u4)
(define-constant condition-flooded u5)
(define-constant condition-unsafe u6)
(define-constant condition-snow u7)
(define-constant condition-maintenance u8)

;; Trail condition maps
(define-map trail-current-conditions
  { trail-id: uint }
  {
    condition-type: uint,
    severity: uint,
    last-updated: uint,
    reporter-count: uint,
    verified: bool,
    expires-at: uint
  }
)

(define-map condition-reports
  { trail-id: uint, reporter: principal, report-id: uint }
  {
    condition-type: uint,
    severity: uint,
    description: (string-ascii 128),
    timestamp: uint,
    verified-by: uint,
    reward-earned: uint
  }
)

(define-map condition-verifications
  { trail-id: uint, report-id: uint, verifier: principal }
  {
    verified: bool,
    timestamp: uint
  }
)

(define-map user-condition-stats
  { user: principal }
  {
    reports-submitted: uint,
    reports-verified: uint,
    verifications-performed: uint,
    condition-rewards-earned: uint,
    accuracy-score: uint
  }
)

(define-data-var condition-report-reward uint u10000)
(define-data-var condition-verification-reward uint u5000)
(define-data-var condition-expiry-blocks uint u1440)
(define-data-var next-report-id uint u1)
(define-data-var min-verifications uint u2)

(define-public (create-trail (name (string-ascii 64)) (location (string-ascii 128)) (difficulty uint) (reward-pool uint) (discovery-reward uint) (maintenance-reward uint))
  (let
    (
      (trail-id (var-get next-trail-id))
      (current-block stacks-block-height)
    )
    (asserts! (> reward-pool u0) err-invalid-amount)
    (asserts! (> discovery-reward u0) err-invalid-amount)
    (asserts! (> maintenance-reward u0) err-invalid-amount)
    (asserts! (<= difficulty u5) err-invalid-amount)
    
    (try! (ft-transfer? trailbit reward-pool tx-sender (as-contract tx-sender)))
    
    (map-set trails
      { trail-id: trail-id }
      {
        name: name,
        location: location,
        difficulty: difficulty,
        creator: tx-sender,
        reward-pool: reward-pool,
        discovery-reward: discovery-reward,
        maintenance-reward: maintenance-reward,
        created-at: current-block,
        active: true,
        total-visits: u0,
        total-ratings: u0,
        average-rating: u0,
        rating-sum: u0
      }
    )
    
    (map-set user-stats
      { user: tx-sender }
      (merge
        (default-to
          { trails-created: u0, trails-visited: u0, maintenance-performed: u0, total-earned: u0 }
          (map-get? user-stats { user: tx-sender })
        )
        { trails-created: (+ (get trails-created (default-to { trails-created: u0, trails-visited: u0, maintenance-performed: u0, total-earned: u0 } (map-get? user-stats { user: tx-sender }))) u1) }
      )
    )
    
    (var-set next-trail-id (+ trail-id u1))
    (ok trail-id)
  )
)

(define-public (visit-trail (trail-id uint))
  (let
    (
      (trail (unwrap! (map-get? trails { trail-id: trail-id }) err-not-found))
      (current-block stacks-block-height)
      (visitor-data (default-to { visit-count: u0, last-visit: u0, total-earned: u0 } (map-get? trail-visits { trail-id: trail-id, visitor: tx-sender })))
      (base-reward (get discovery-reward trail))
      (quality-multiplier (if (>= (get average-rating trail) (var-get quality-threshold)) (var-get bonus-multiplier) u100))
      (safety-multiplier (get-condition-safety-multiplier trail-id))
      (combined-multiplier (/ (* quality-multiplier safety-multiplier) u100))
      (reward-amount (/ (* base-reward combined-multiplier) u100))
    )
    (asserts! (get active trail) err-trail-inactive)
    (asserts! (>= (get reward-pool trail) reward-amount) err-insufficient-funds)
    (asserts! (> (- current-block (get last-visit visitor-data)) u144) err-unauthorized)
    
    (try! (as-contract (ft-transfer? trailbit reward-amount tx-sender tx-sender)))
    
    (map-set trails
      { trail-id: trail-id }
      (merge trail {
        reward-pool: (- (get reward-pool trail) reward-amount),
        total-visits: (+ (get total-visits trail) u1)
      })
    )
    
    (map-set trail-visits
      { trail-id: trail-id, visitor: tx-sender }
      {
        visit-count: (+ (get visit-count visitor-data) u1),
        last-visit: current-block,
        total-earned: (+ (get total-earned visitor-data) reward-amount)
      }
    )
    
    (map-set user-stats
      { user: tx-sender }
      (merge
        (default-to
          { trails-created: u0, trails-visited: u0, maintenance-performed: u0, total-earned: u0 }
          (map-get? user-stats { user: tx-sender })
        )
        {
          trails-visited: (+ (get trails-visited (default-to { trails-created: u0, trails-visited: u0, maintenance-performed: u0, total-earned: u0 } (map-get? user-stats { user: tx-sender }))) u1),
          total-earned: (+ (get total-earned (default-to { trails-created: u0, trails-visited: u0, maintenance-performed: u0, total-earned: u0 } (map-get? user-stats { user: tx-sender }))) reward-amount)
        }
      )
    )
    
    (ok reward-amount)
  )
)

(define-public (maintain-trail (trail-id uint))
  (let
    (
      (trail (unwrap! (map-get? trails { trail-id: trail-id }) err-not-found))
      (current-block stacks-block-height)
      (maintainer-data (default-to { maintenance-count: u0, last-maintenance: u0, total-earned: u0 } (map-get? trail-maintenance { trail-id: trail-id, maintainer: tx-sender })))
      (reward-amount (get maintenance-reward trail))
    )
    (asserts! (get active trail) err-trail-inactive)
    (asserts! (>= (get reward-pool trail) reward-amount) err-insufficient-funds)
    (asserts! (> (- current-block (get last-maintenance maintainer-data)) u1008) err-unauthorized)
    
    (try! (as-contract (ft-transfer? trailbit reward-amount tx-sender tx-sender)))
    
    (map-set trails
      { trail-id: trail-id }
      (merge trail {
        reward-pool: (- (get reward-pool trail) reward-amount)
      })
    )
    
    (map-set trail-maintenance
      { trail-id: trail-id, maintainer: tx-sender }
      {
        maintenance-count: (+ (get maintenance-count maintainer-data) u1),
        last-maintenance: current-block,
        total-earned: (+ (get total-earned maintainer-data) reward-amount)
      }
    )
    
    (map-set user-stats
      { user: tx-sender }
      (merge
        (default-to
          { trails-created: u0, trails-visited: u0, maintenance-performed: u0, total-earned: u0 }
          (map-get? user-stats { user: tx-sender })
        )
        {
          maintenance-performed: (+ (get maintenance-performed (default-to { trails-created: u0, trails-visited: u0, maintenance-performed: u0, total-earned: u0 } (map-get? user-stats { user: tx-sender }))) u1),
          total-earned: (+ (get total-earned (default-to { trails-created: u0, trails-visited: u0, maintenance-performed: u0, total-earned: u0 } (map-get? user-stats { user: tx-sender }))) reward-amount)
        }
      )
    )
    
    (ok reward-amount)
  )
)

(define-public (fund-trail (trail-id uint) (amount uint))
  (let
    (
      (trail (unwrap! (map-get? trails { trail-id: trail-id }) err-not-found))
    )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (get active trail) err-trail-inactive)
    
    (try! (ft-transfer? trailbit amount tx-sender (as-contract tx-sender)))
    
    (map-set trails
      { trail-id: trail-id }
      (merge trail {
        reward-pool: (+ (get reward-pool trail) amount)
      })
    )
    
    (ok true)
  )
)

(define-public (deactivate-trail (trail-id uint))
  (let
    (
      (trail (unwrap! (map-get? trails { trail-id: trail-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get creator trail)) err-unauthorized)
    
    (map-set trails
      { trail-id: trail-id }
      (merge trail { active: false })
    )
    
    (ok true)
  )
)

(define-public (rate-trail (trail-id uint) (rating uint) (review (string-ascii 256)))
  (let
    (
      (trail (unwrap! (map-get? trails { trail-id: trail-id }) err-not-found))
      (current-block stacks-block-height)
      (existing-rating (map-get? trail-ratings { trail-id: trail-id, rater: tx-sender }))
      (user-review-data (default-to { reviews-given: u0, helpful-votes-received: u0, reviewer-level: u0 } (map-get? user-review-stats { user: tx-sender })))
      (trail-review-data (default-to { review-count: u0, featured-review: none, featured-reviewer: none } (map-get? trail-reviews { trail-id: trail-id })))
    )
    (asserts! (is-none existing-rating) err-already-rated)
    (asserts! (not (is-eq tx-sender (get creator trail))) err-cannot-rate-own-trail)
    (asserts! (>= rating (var-get min-rating)) err-invalid-rating)
    (asserts! (<= rating (var-get max-rating)) err-invalid-rating)
    (asserts! (<= (len review) u256) err-review-too-long)
    (asserts! (get active trail) err-trail-inactive)
    
    (map-set trail-ratings
      { trail-id: trail-id, rater: tx-sender }
      {
        rating: rating,
        review: review,
        timestamp: current-block,
        helpful-votes: u0
      }
    )
    
    (let
      (
        (new-total-ratings (+ (get total-ratings trail) u1))
        (new-rating-sum (+ (get rating-sum trail) rating))
        (new-average-rating (/ new-rating-sum new-total-ratings))
      )
      (map-set trails
        { trail-id: trail-id }
        (merge trail {
          total-ratings: new-total-ratings,
          rating-sum: new-rating-sum,
          average-rating: new-average-rating
        })
      )
    )
    
    (map-set trail-reviews
      { trail-id: trail-id }
      (merge trail-review-data {
        review-count: (+ (get review-count trail-review-data) u1),
        featured-review: (if (> (len review) u0) (some review) (get featured-review trail-review-data)),
        featured-reviewer: (if (> (len review) u0) (some tx-sender) (get featured-reviewer trail-review-data))
      })
    )
    
    (map-set user-review-stats
      { user: tx-sender }
      {
        reviews-given: (+ (get reviews-given user-review-data) u1),
        helpful-votes-received: (get helpful-votes-received user-review-data),
        reviewer-level: (calculate-reviewer-level (+ (get reviews-given user-review-data) u1) (get helpful-votes-received user-review-data))
      }
    )
    
    (ok true)
  )
)

(define-public (vote-review-helpful (trail-id uint) (reviewer principal) (helpful bool))
  (let
    (
      (existing-vote (map-get? review-helpfulness { trail-id: trail-id, reviewer: reviewer, voter: tx-sender }))
      (rating-data (unwrap! (map-get? trail-ratings { trail-id: trail-id, rater: reviewer }) err-not-found))
      (reviewer-stats (default-to { reviews-given: u0, helpful-votes-received: u0, reviewer-level: u0 } (map-get? user-review-stats { user: reviewer })))
    )
    (asserts! (is-none existing-vote) err-already-exists)
    (asserts! (not (is-eq tx-sender reviewer)) err-unauthorized)
    
    (map-set review-helpfulness
      { trail-id: trail-id, reviewer: reviewer, voter: tx-sender }
      {
        voted: true,
        vote-type: helpful
      }
    )
    
    (if helpful
      (begin
        (map-set trail-ratings
          { trail-id: trail-id, rater: reviewer }
          (merge rating-data {
            helpful-votes: (+ (get helpful-votes rating-data) u1)
          })
        )
        (map-set user-review-stats
          { user: reviewer }
          {
            reviews-given: (get reviews-given reviewer-stats),
            helpful-votes-received: (+ (get helpful-votes-received reviewer-stats) u1),
            reviewer-level: (calculate-reviewer-level (get reviews-given reviewer-stats) (+ (get helpful-votes-received reviewer-stats) u1))
          }
        )
        (ok true)
      )
      (ok true)
    )
  )
)

(define-public (update-featured-review (trail-id uint) (new-reviewer principal))
  (let
    (
      (trail (unwrap! (map-get? trails { trail-id: trail-id }) err-not-found))
      (rating-data (unwrap! (map-get? trail-ratings { trail-id: trail-id, rater: new-reviewer }) err-not-found))
      (trail-review-data (default-to { review-count: u0, featured-review: none, featured-reviewer: none } (map-get? trail-reviews { trail-id: trail-id })))
    )
    (asserts! (is-eq tx-sender (get creator trail)) err-unauthorized)
    (asserts! (> (len (get review rating-data)) u0) err-not-found)
    
    (map-set trail-reviews
      { trail-id: trail-id }
      (merge trail-review-data {
        featured-review: (some (get review rating-data)),
        featured-reviewer: (some new-reviewer)
      })
    )
    
    (ok true)
  )
)

(define-private (calculate-reviewer-level (reviews-given uint) (helpful-votes uint))
  (let
    (
      (total-points (+ reviews-given (* helpful-votes u2)))
    )
    (if (>= total-points u50)
      u5
      (if (>= total-points u25)
        u4
        (if (>= total-points u10)
          u3
          (if (>= total-points u5)
            u2
            u1
          )
        )
      )
    )
  )
)

;; Trail condition reporting functions
(define-public (report-trail-condition (trail-id uint) (condition-type uint) (severity uint) (description (string-ascii 128)))
  (let
    (
      (trail (unwrap! (map-get? trails { trail-id: trail-id }) err-not-found))
      (current-block stacks-block-height)
      (report-id (var-get next-report-id))
      (existing-condition (map-get? trail-current-conditions { trail-id: trail-id }))
      (user-condition-data (default-to { reports-submitted: u0, reports-verified: u0, verifications-performed: u0, condition-rewards-earned: u0, accuracy-score: u100 } (map-get? user-condition-stats { user: tx-sender })))
      (expires-at (+ current-block (var-get condition-expiry-blocks)))
    )
    ;; Validate inputs
    (asserts! (get active trail) err-trail-inactive)
    (asserts! (and (>= condition-type condition-clear) (<= condition-type condition-maintenance)) err-invalid-condition-type)
    (asserts! (and (>= severity u1) (<= severity u5)) err-invalid-amount)
    (asserts! (<= (len description) u128) err-review-too-long)
    
    ;; Check if condition is already reported recently
    (match existing-condition
      condition-data
        (asserts! (> (- current-block (get last-updated condition-data)) u144) err-condition-already-reported)
      true
    )
    
    ;; Create condition report
    (map-set condition-reports
      { trail-id: trail-id, reporter: tx-sender, report-id: report-id }
      {
        condition-type: condition-type,
        severity: severity,
        description: description,
        timestamp: current-block,
        verified-by: u0,
        reward-earned: u0
      }
    )
    
    ;; Update or create current trail condition
    (map-set trail-current-conditions
      { trail-id: trail-id }
      {
        condition-type: condition-type,
        severity: severity,
        last-updated: current-block,
        reporter-count: u1,
        verified: false,
        expires-at: expires-at
      }
    )
    
    ;; Update user stats
    (map-set user-condition-stats
      { user: tx-sender }
      (merge user-condition-data {
        reports-submitted: (+ (get reports-submitted user-condition-data) u1)
      })
    )
    
    ;; Award initial reporting reward
    (try! (as-contract (ft-transfer? trailbit (var-get condition-report-reward) tx-sender tx-sender)))
    
    (var-set next-report-id (+ report-id u1))
    (ok report-id)
  )
)

(define-public (verify-condition-report (trail-id uint) (reporter principal) (report-id uint) (agrees bool))
  (let
    (
      (current-block stacks-block-height)
      (report-data (unwrap! (map-get? condition-reports { trail-id: trail-id, reporter: reporter, report-id: report-id }) err-not-found))
      (condition-data (unwrap! (map-get? trail-current-conditions { trail-id: trail-id }) err-condition-not-found))
      (existing-verification (map-get? condition-verifications { trail-id: trail-id, report-id: report-id, verifier: tx-sender }))
      (user-verification-data (default-to { reports-submitted: u0, reports-verified: u0, verifications-performed: u0, condition-rewards-earned: u0, accuracy-score: u100 } (map-get? user-condition-stats { user: tx-sender })))
    )
    ;; Validate verification
    (asserts! (is-none existing-verification) err-already-exists)
    (asserts! (not (is-eq tx-sender reporter)) err-cannot-verify-own-report)
    (asserts! (< (get timestamp report-data) current-block) err-condition-expired)
    (asserts! (< current-block (get expires-at condition-data)) err-condition-expired)
    
    ;; Create verification record
    (map-set condition-verifications
      { trail-id: trail-id, report-id: report-id, verifier: tx-sender }
      {
        verified: agrees,
        timestamp: current-block
      }
    )
    
    ;; Update user verification stats
    (map-set user-condition-stats
      { user: tx-sender }
      (merge user-verification-data {
        verifications-performed: (+ (get verifications-performed user-verification-data) u1)
      })
    )
    
    ;; Award verification reward
    (try! (as-contract (ft-transfer? trailbit (var-get condition-verification-reward) tx-sender tx-sender)))
    
    ;; Check if enough verifications to mark as verified
    (let
      (
        (verification-count (+ (get verified-by report-data) (if agrees u1 u0)))
      )
      (if (>= verification-count (var-get min-verifications))
        (begin
          (map-set trail-current-conditions
            { trail-id: trail-id }
            (merge condition-data { verified: true })
          )
          (map-set condition-reports
            { trail-id: trail-id, reporter: reporter, report-id: report-id }
            (merge report-data { verified-by: verification-count })
          )
        )
        (map-set condition-reports
          { trail-id: trail-id, reporter: reporter, report-id: report-id }
          (merge report-data { verified-by: verification-count })
        )
      )
    )
    
    (ok agrees)
  )
)

(define-public (clear-expired-conditions (trail-id uint))
  (let
    (
      (current-block stacks-block-height)
      (condition-data (map-get? trail-current-conditions { trail-id: trail-id }))
    )
    (match condition-data
      data
        (if (>= current-block (get expires-at data))
          (begin
            (map-delete trail-current-conditions { trail-id: trail-id })
            (ok true)
          )
          err-condition-not-found
        )
      err-condition-not-found
    )
  )
)

;; Helper function to calculate condition impact on rewards
(define-private (get-condition-safety-multiplier (trail-id uint))
  (let
    (
      (condition-data (map-get? trail-current-conditions { trail-id: trail-id }))
      (current-block stacks-block-height)
    )
    (match condition-data
      data
        (if (< current-block (get expires-at data))
          (if (get verified data)
            (if (or (is-eq (get condition-type data) condition-unsafe) (is-eq (get condition-type data) condition-blocked))
              u0  ;; No rewards for unsafe/blocked trails
              (if (or (is-eq (get condition-type data) condition-muddy) (is-eq (get condition-type data) condition-icy))
                u75  ;; 25% penalty for challenging conditions
                u100  ;; Normal rewards for clear/snow/maintenance
              )
            )
            u90  ;; 10% penalty for unverified conditions
          )
          u100  ;; Normal rewards if no current conditions
        )
      u100  ;; Normal rewards if no condition data
    )
  )
)

(define-public (mint (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (try! (ft-mint? trailbit amount recipient))
    (var-set total-supply (+ (var-get total-supply) amount))
    (ok true)
  )
)

(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
  (begin
    (asserts! (is-eq tx-sender sender) err-unauthorized)
    (ft-transfer? trailbit amount sender recipient)
  )
)

(define-read-only (get-balance (account principal))
  (ok (ft-get-balance trailbit account))
)

(define-read-only (get-total-supply)
  (ok (ft-get-supply trailbit))
)

(define-read-only (get-name)
  (ok (var-get token-name))
)

(define-read-only (get-symbol)
  (ok (var-get token-symbol))
)

(define-read-only (get-decimals)
  (ok (var-get token-decimals))
)

(define-read-only (get-token-uri)
  (ok none)
)

(define-read-only (get-trail (trail-id uint))
  (map-get? trails { trail-id: trail-id })
)

(define-read-only (get-trail-visits (trail-id uint) (visitor principal))
  (map-get? trail-visits { trail-id: trail-id, visitor: visitor })
)

(define-read-only (get-trail-maintenance (trail-id uint) (maintainer principal))
  (map-get? trail-maintenance { trail-id: trail-id, maintainer: maintainer })
)

(define-read-only (get-user-stats (user principal))
  (map-get? user-stats { user: user })
)

(define-read-only (get-next-trail-id)
  (var-get next-trail-id)
)

(define-read-only (get-trail-rating (trail-id uint) (rater principal))
  (map-get? trail-ratings { trail-id: trail-id, rater: rater })
)

(define-read-only (get-trail-reviews (trail-id uint))
  (map-get? trail-reviews { trail-id: trail-id })
)

(define-read-only (get-user-review-stats (user principal))
  (map-get? user-review-stats { user: user })
)

(define-read-only (get-review-helpfulness (trail-id uint) (reviewer principal) (voter principal))
  (map-get? review-helpfulness { trail-id: trail-id, reviewer: reviewer, voter: voter })
)

(define-read-only (get-quality-multiplier (trail-id uint))
  (let
    (
      (trail (map-get? trails { trail-id: trail-id }))
    )
    (match trail
      trail-data
        (if (>= (get average-rating trail-data) (var-get quality-threshold))
          (var-get bonus-multiplier)
          u100
        )
      u100
    )
  )
)

(define-read-only (get-rating-settings)
  {
    min-rating: (var-get min-rating),
    max-rating: (var-get max-rating),
    quality-threshold: (var-get quality-threshold),
    bonus-multiplier: (var-get bonus-multiplier)
  }
)

;; Condition reporting read-only functions
(define-read-only (get-trail-current-condition (trail-id uint))
  (map-get? trail-current-conditions { trail-id: trail-id })
)

(define-read-only (get-condition-report (trail-id uint) (reporter principal) (report-id uint))
  (map-get? condition-reports { trail-id: trail-id, reporter: reporter, report-id: report-id })
)

(define-read-only (get-condition-verification (trail-id uint) (report-id uint) (verifier principal))
  (map-get? condition-verifications { trail-id: trail-id, report-id: report-id, verifier: verifier })
)

(define-read-only (get-user-condition-stats (user principal))
  (map-get? user-condition-stats { user: user })
)

(define-read-only (get-condition-settings)
  {
    report-reward: (var-get condition-report-reward),
    verification-reward: (var-get condition-verification-reward),
    expiry-blocks: (var-get condition-expiry-blocks),
    min-verifications: (var-get min-verifications),
    next-report-id: (var-get next-report-id)
  }
)

(define-read-only (get-condition-types)
  {
    clear: condition-clear,
    muddy: condition-muddy,
    icy: condition-icy,
    blocked: condition-blocked,
    flooded: condition-flooded,
    unsafe: condition-unsafe,
    snow: condition-snow,
    maintenance: condition-maintenance
  }
)

(mint u1000000000000 contract-owner)


