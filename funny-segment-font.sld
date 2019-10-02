
(define-library (funny-segment-font)

  (export gen-funny-font
          render-glyph
          char->glyph-index)

  (import (scheme base))
  (import (srfi 4))
  (import (srfi 69))

  (begin
    (define-record-type
      funny-font
      make-funny-font
      funny-font?

      segment-width
      segment-height

      segments
      glyphs)

    (define-record-type
      funny-font-glyph
      make-funny-font-glyph
      funny-font-glyph?

      width
      segment-indexes)

    (define *glyph-tbl*
      (alist->hash-table
        `((#\0 . 0) (#\1 . 1) (#\2 . 2) (#\3 . 3) (#\4 . 4)
          (#\5 . 5) (#\6 . 6) (#\7 . 7) (#\8 . 8) (#\9 . 9)
          (#\: . 10) (#\space . 11))))


    (define (gen-segments w h pad)
      (let* ((pad-w (+ pad w))
             (pad-h (+ pad h))
             (pad-pad-h (+ pad pad-h))
             (pad-pad-h-h (+ pad-pad-h h))
             (dot-x (exact (floor (* 0.5 pad-w)))))
        (vector
          (vector 'hori pad 0)
          (vector 'vert pad-w pad)
          (vector 'vert pad-w pad-pad-h)
          (vector 'hori pad pad-pad-h-h)
          (vector 'vert 0 pad-pad-h)
          (vector 'vert 0 pad)
          (vector 'hori pad pad-h)

          ;; Middle dot.
          (vector 'dot dot-x (exact (floor (* 0.5 pad-pad-h))))
          (vector 'dot dot-x (exact (floor (* 1.5 pad-h)))))))

    (define (gen-funny-font width height pad)
      (let ((segments (gen-segments width height pad))
            (digit-width (+ width pad pad pad)))
        (make-funny-font
          width height
          segments
          (vector
            ;; digits
            (make-funny-font-glyph digit-width #(0 1 2 3 4 5))
            (make-funny-font-glyph digit-width #(1 2))
            (make-funny-font-glyph digit-width #(0 1 3 4 6))
            (make-funny-font-glyph digit-width #(0 1 2 3 6))
            (make-funny-font-glyph digit-width #(1 2 5 6))
            (make-funny-font-glyph digit-width #(0 2 3 5 6))
            (make-funny-font-glyph digit-width #(0 2 3 4 5 6))
            (make-funny-font-glyph digit-width #(0 1 2))
            (make-funny-font-glyph digit-width #(0 1 2 3 4 5 6))
            (make-funny-font-glyph digit-width #(0 1 2 3 5 6))

            ;; Colon and Space
            (make-funny-font-glyph digit-width #(7 8))
            (make-funny-font-glyph digit-width #())))))

    (define (char->glyph-index chr)
      (hash-table-ref/default *glyph-tbl* chr #f))

    (define (render-glyph font glyph-index scale x y)
      (and (< glyph-index (vector-length (funny-font-glyphs font)))
           (let ((glyph (vector-ref (funny-font-glyphs font) glyph-index))
                 (seg-w (funny-font-segment-width font))
                 (seg-h (funny-font-segment-height font))
                 (segments (funny-font-segments font)))
             (values (vector-map
                       (lambda (segment-index)
                         (let* ((seg (vector-ref segments segment-index))
                                (seg-type (vector-ref seg 0))
                                (seg-x (vector-ref seg 1))
                                (seg-y (vector-ref seg 2))

                                ;; rectangle components
                                (s-x (* scale (+ x seg-x)))
                                (s-y (* scale (+ y seg-y)))
                                (s-w (if (eq? seg-type 'hori)
                                         (* scale seg-w) scale))
                                (s-h (if (eq? seg-type 'vert)
                                         (* scale seg-h) scale)))
                           (s32vector s-x s-y s-w s-h)))
                       (funny-font-glyph-segment-indexes glyph))
                     ;; width as second value
                     (funny-font-glyph-width glyph)))))))









