def no_op(): Unit = {
  println("Encerrando o programa")
  return
}

def print_text(data: Seq[(String, Int)]): Unit = {
  println("Printando o resultado")
  for (w <- data) println(w._1 + " - " + w._2)
  no_op()
}

def sort(data: Seq[(String, Int)]): Unit = {
  println("Executando sort")
  print_text(data.sortWith(_._2 > _._2))
}

def frequencies(data: List[String]): Unit = {
  println("Executando frequencies")
  sort(data.groupBy(identity).mapValues(_.size).toSeq)
}

def remove_stop_words(data: Array[String]): Unit = {
  println("Executando remove_stop_words")
  var stop_words = scala.io.Source.fromFile("C:\\Users\\Felipe\\AppData\\Roaming\\JetBrains\\IdeaIC2020.3\\scratches\\stop_words.txt").mkString
  stop_words = stop_words.replaceAll("[^A-Za-z0-9 ]", " ")
  val stop_words_list = stop_words.split(" +")
  var new_data = List.empty[String]

  for (i <- data) {
    var keep = true
    for (j <- stop_words_list) {
      if (i == j) {
        keep = false
      }
    }
    if (keep == true){
      new_data = new_data ++ List(i)
    }
  }

  frequencies(new_data)
}

def scan(data: String): Unit = {
  println("Executando scan")
  val word_list = data.split(" +")
  remove_stop_words(word_list)
}

def normalize(data: String, func:(String => Unit)): Unit = {
  println("Executando normalize")
  func(data.toLowerCase())
}

def filter_chars(data: String, func:(String, String => Unit) => Unit): Unit = {
  println("Executando filter_chars")
  func(data.replaceAll("[^A-Za-z0-9 ]", " "), scan)
}

def read_file(path: String, func:(String, (String, String => Unit) => Unit) => Unit) = {
  println("Executando read_file")
  func(scala.io.Source.fromFile(path).mkString, normalize)
}

println("Iniciando o programa")
read_file("C:\\Users\\Felipe\\AppData\\Roaming\\JetBrains\\IdeaIC2020.3\\scratches\\text.txt", filter_chars)