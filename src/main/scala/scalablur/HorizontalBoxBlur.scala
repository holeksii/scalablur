package scalablur

object HorizontalBoxBlur extends BoxBlur:

  /** Blurs the rows of the source image `src` into the destination image `dst`,
    * starting with `from` and ending with `end` (non-inclusive).
    *
    * Within each row, `blur` traverses the pixels by going from left to right.
    */
  def blurSegment(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit =
    for
      y <- from until end
      x <- 0 until src.width
    do dst.update(x, y, boxBlurKernel(src, x, y, radius))

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
    *
    * Parallelization is done by stripping the source image `src` into
    * `numTasks` separate strips, where each strip is composed of some number of
    * rows.
    */
  def blur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit =
    val step = src.height / numTasks
    val tasks =
      for i <- 0 until numTasks
      yield
        if i == numTasks - 1 then
          task(blurSegment(src, dst, i * step, src.height, radius))
        else task(blurSegment(src, dst, i * step, (i + 1) * step, radius))
    tasks.foreach(_.join())
