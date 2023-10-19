package scalablur

object VerticalBoxBlur extends BoxBlur:

  /** Blurs the columns of the source image `src` into the destination image
    * `dst`, starting with `from` and ending with `end` (non-inclusive).
    *
    * Within each column, `blur` traverses the pixels by going from top to
    * bottom.
    */
  def blurSegment(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit =
    for
      x <- from until end
      y <- 0 until src.height
    do dst.update(x, y, boxBlurKernel(src, x, y, radius))

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
    *
    * Parallelization is done by stripping the source image `src` into
    * `numTasks` separate strips, where each strip is composed of some number of
    * columns.
    */
  def blur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit =
    val step = src.width / numTasks
    val tasks =
      for i <- 0 until numTasks
      yield
        if i == numTasks - 1 then
          task(blurSegment(src, dst, i * step, src.width, radius))
        else task(blurSegment(src, dst, i * step, (i + 1) * step, radius))
    tasks.foreach(_.join())
