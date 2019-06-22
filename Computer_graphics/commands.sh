wget "https://raw.githubusercontent.com/ZhekehZ/Stepik/master/CV/plate.svg" -O plate.svg 
ffmpeg -i input_video.mp4 -ss 00:00:30 -t 00:00:10 -c copy cropped.mp4
convert -resize 50% -depth 8 -background transparent plate.svg plate.png
ffmpeg -i cropped.mp4 -i plate.png -filter_complex "overlay=0:446:enable=between(t\,1\,9)" plated.mp4
ffmpeg -i plated.mp4 -vf drawtext="fontfile=Arial.ttf:text='text': fontcolor=black: fontsize=24: x=200: y=476: enable=between(t\,2\,9)" result.mp4
