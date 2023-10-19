package scalablur

trait BoxBlur:
  def blurSegment(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit
  def blur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit
