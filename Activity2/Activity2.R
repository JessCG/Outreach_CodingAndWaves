# Set up a square plot area
par(pty="s")
plot(1:10, type = 'n', main = "Can you reproduce this star?", 
     xlab = "x", ylab = "y", xlim = c(0, 10), ylim = c(0, 10), 
     axes = FALSE, asp = 1)

# Fix axes
axis(1, at = seq(0, 10))
axis(2, at = seq(0, 10))

# Add guiding lines
abline(h = seq(0, 10), v = seq(0, 10), col = "grey", lty = 2)

############### Add your new lines of code below ###############
# Here is an example of what to use:
# segments(x1, y1, x2, y2, col = "color", lwd = 1)

# Save your work
dev.copy(png, "yourStar.png", 
         width = 700, height = 700)
dev.off()




