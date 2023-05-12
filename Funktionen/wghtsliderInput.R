# function to change sliderInput
wghtsliderInput = function(inputId,value, label, submitted=FALSE) {
  if (!submitted)
    sliderInput(inputId=inputId,
                value=value,
                label=label,
                min=0,
                max=1,
                ticks=FALSE)
}