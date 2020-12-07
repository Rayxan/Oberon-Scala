class multivariavel {
  var data_string: String = ""
  var data_array: Array[String] = Array.empty[String]
  var data_list: List[String] = List.empty[String]
  var data_seq: Seq[(String, Int)] = Seq.empty[(String, Int)]
}

trait Step {
  def process(data:multivariavel):multivariavel
  def run(data:multivariavel, next:Step):multivariavel
}

abstract class MidStep (next2:Step) extends Step {
  def run(data:multivariavel, next:Step):multivariavel = next.run(process(data),next2)
}

abstract class EndStep extends Step {
  def run(data:multivariavel, next:Step):multivariavel = process(data)
}

object no_op extends EndStep {
  def process(data:multivariavel):multivariavel = null
}

object print_text extends MidStep (null) {
  def process(data:multivariavel):multivariavel = {
    for (w <- data.data_seq) println(w._1 + " - " + w._2)
    return data
  }
}

object sort extends MidStep (no_op) {
  def process(data:multivariavel):multivariavel = {
    data.data_seq = data.data_seq.sortWith(_._2 > _._2)
    return data
  }
}

object frequencies extends MidStep (print_text) {
  def process(data:multivariavel):multivariavel = {
    data.data_seq = data.data_list.groupBy(identity).mapValues(_.size).toSeq
    return data
  }
}

object remove_stop_words extends MidStep (sort) {
  def process(data:multivariavel):multivariavel = {

    var stop_words = scala.io.Source.fromFile("C:\\Users\\Felipe\\Desktop\\WordCounter\\stop_words.txt").mkString
    stop_words = stop_words.replaceAll("[^A-Za-z0-9 ]", " ")
    val stop_words_list = stop_words.split(" +")

    for (i <- data.data_array) {
      var keep = true
      for (j <- stop_words_list) {
        if (i == j) {
          keep = false
        }
      }
      if (keep == true){
        data.data_list = data.data_list ++ List(i)
      }
    }

    return data
  }
}

object scan extends MidStep (frequencies) {
  def process(data:multivariavel):multivariavel = {
    data.data_array = data.data_string.split(" +")
    return data
  }
}

object normalize extends MidStep (remove_stop_words) {
  def process(data:multivariavel):multivariavel = {
    data.data_string = data.data_string.toLowerCase()
    return data
  }
}

object filter_chars extends MidStep (scan) {
  def process(data:multivariavel):multivariavel = {
    data.data_string = data.data_string.replaceAll("[^A-Za-z0-9 ]", " ")
    return data
  }
}

object read_file extends MidStep (normalize) {
  def process(data:multivariavel):multivariavel = {
    data.data_string = scala.io.Source.fromFile(data.data_string).mkString
    return data
  }
}

var x = read_file

var entrada = new multivariavel
entrada.data_string = "C:\\Users\\Felipe\\Desktop\\WordCounter\\text.txt"
println(read_file.run(entrada,filter_chars))