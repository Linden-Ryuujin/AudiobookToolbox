;nyquist plug-in
;version 4
;type tool
;name "Label all tracks with name"
;release 2.3.2
;author "Linden Ryuujin"
;copyright "MIT"

(let ((tracks (aud-get-info "tracks")))
  (dotimes (j (length tracks))
    (setf track (nth j tracks))
    (aud-do (format nil "SelectTracks: Track=~s TrackCount=1 Mode=Set" j))
    (aud-do "CursTrackStart:")
    (aud-do "SelCursorToTrackEnd:")
    (aud-do "AddLabel:")
    (aud-do (format nil "SetLabel: Label=~s Text=~s" j (second (assoc 'name track))))
  )
)