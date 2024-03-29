
days <- 420
start.date <- Sys.Date() - days

for (i in 1:days) {
  date <- start.date + i
  predictProphet('ATVI', from.date='2014-01-01', to.date=date, png.filename=paste0('tickers/atvi/forecast/', date, '.png'))
}

# ffmpeg -r 60 -s 1920x1080 -pattern_type glob -i '*.png' -vcodec libx264 -crf 25 -pix_fmt yuv420p output.mp4