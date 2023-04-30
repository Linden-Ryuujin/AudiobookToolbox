;nyquist plug-in
;version 4
;type tool
;name "Label and Flatten Tracks"
;release 2.3.2
;author "Linden Ryuujin"
;copyright "MIT"

(aud-do "SelectAll:")
(aud-do "Align_EndToEnd:")

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

(aud-do "SelectTracks: Track=0 TrackCount=1 Mode=Set")
(aud-do "SelectAll:")
(aud-do "MixAndRender:")