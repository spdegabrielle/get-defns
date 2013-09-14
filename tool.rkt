#lang racket/gui
(require drracket/tool)

(provide tool@)

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    (define (phase1) (void))
    (define (phase2) (void))
    
    
;    (define/public (language-changed new-language vertical?)
;      (set! define-popup-capability-info
;            (send new-language capability-value 'drscheme:define-popup))
;      (let ([define-name (get-define-popup-name define-popup-capability-info
;                                                vertical?)])
;        (cond
;          [define-name
;            (set-message #f define-name)
;            (set-hidden? #f)]
;          [else
;           (set-hidden? #t)])))
    
    (define/override (fill-popup menu reset)
      (when define-popup-capability-info
        (let* ([text (send frame get-definitions-text)]
               [unsorted-defns (get-definitions (car define-popup-capability-info)
                                                (not sort-by-name?)
                                                text)]
               [defns (if sort-by-name?
                          (sort
                           unsorted-defns
                           (λ (x y) (string-ci<=? (defn-name x) (defn-name y))))
                          unsorted-defns)])
          (make-object menu:can-restore-menu-item% sorting-name
            menu
            (λ (x y)
              (change-sorting-order)))
          (make-object separator-menu-item% menu)
          (if (null? defns)
              (send (make-object menu:can-restore-menu-item%
                      (string-constant no-definitions-found)
                      menu
                      void)
                    enable #f)
              (let loop ([defns defns])
                (unless (null? defns)
                  (let* ([defn (car defns)]
                         [checked? 
                          (let ([t-start (send text get-start-position)]
                                [t-end (send text get-end-position)]
                                [d-start (defn-start-pos defn)]
                                [d-end (defn-end-pos defn)])
                            (or (<= t-start d-start t-end)
                                (<= t-start d-end t-end)
                                (<= d-start t-start t-end d-end)))]
                         [item
                          (make-object (if checked?
                                           menu:can-restore-checkable-menu-item%
                                           menu:can-restore-menu-item%)
                            (gui-utils:quote-literal-label (defn-name defn))
                            
                            menu
                            (λ (x y)
                              (reset)
                              (send text set-position (defn-start-pos defn) (defn-start-pos defn))
                              (let ([canvas (send text get-canvas)])
                                (when canvas
                                  (send canvas focus)))))])
                    (when checked?
                      (send item check #t))
                    (loop (cdr defns)))))))))
    
    
    
    
    
    (define-local-member-name update-counts get-counts-txt)
    
    (define tab-mixin (mixin (drracket:unit:tab<%>)()
                        
                        (define txt (new text%))
                        (define/public (get-defs-txt) txt)
                        (define/public (update-counts lst)
                          (send txt begin-edit-sequence)
                          (send txt erase)
                          (for ([lst (in-list lst)])
                            (let ([c (list-ref lst 0)]
                                  [v (list-ref lst 1)])
                              (send txt insert (format "~s ~s\n" c v))))
                          (send txt end-edit-sequence))
                        (super-new)))
    
    (define (defs-mixin super%)
      (class super%
        (inherit get-tab last-position get-character define-popup-capability-info)
        (define/augment (after-insert start len)
          (inner (void) after-insert start len)
          (update-counts))
        (define/augment (after-delete start len)
          (inner (void) after-delete start len)
          (update-counts))
        (define/augment (after-load-file success?)
          (inner (void) after-load-file success?)
          (update-counts))
        
        ;; get the text
        (define/private (update-counts)
          (let ([ht (make-hash)])
            (for ([i (in-range 0 (last-position))])
              (let ([c (get-character i)])
                (hash-set! ht c (+ 1 (hash-ref ht c 0)))))
            (send (get-tab) update-counts
                  (sort (hash-map ht list) cmp))))
        (super-new)))
    
    (define (cmp lst1 lst2)
      (cond
        [(= (list-ref lst1 1) (list-ref lst2 1))
         (char<=? (list-ref lst1 0) (list-ref lst2 0))]
        [else
         (> (list-ref lst1 1) (list-ref lst2 1))]))
    
    (define frame-mixin (mixin (drracket:unit:frame<%>)()
                          (inherit get-current-tab get-menu-bar get-button-panel
                                   register-toolbar-button)
                          
                          (define freq-canvas #f)
                          (define freq-panel-parent #f)
                          (define/override (make-root-area-container cls parent)
                            (set! freq-panel-parent (super make-root-area-container
                                                           horizontal-panel% parent))
                            (let ([root (make-object cls freq-panel-parent)])
                              (set! freq-canvas (new editor-canvas%
                                                     [parent freq-panel-parent]
                                                     [stretchable-width #f]
                                                     [min-width 200]))
                              root))
                          
                          (define/augment (on-tab-change from-tab to-tab)
                            (inner (void) on-tab-change from-tab to-tab)
                            (send freq-canvas set-editor (send to-tab get-counts-txt)))
                          
                          (super-new)
                          (send freq-canvas set-editor (send (get-current-tab) get-defs-txt))))
    
    (drracket:get/extend:extend-unit-frame frame-mixin)
    (drracket:get/extend:extend-tab tab-mixin)
    (drracket:get/extend:extend-definitions-text defs-mixin)))
