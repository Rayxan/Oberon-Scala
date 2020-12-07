// alterar depois
def no_op(func: () => Unit): Unit = {
  println("Encerrando o programa")
  return
}

// alterar depois
def print_text(data: Seq[(String, Int)], func: (() => Unit) => Unit): Unit = {
  println("Printando o resultado")
  for (w <- data) println(w._1 + " - " + w._2)
  func(null)
}

// func: (Seq[(String, Int)]) => Unit
def sort(data: Seq[(String, Int)], func: (Seq[(String, Int)], (() => Unit) => Unit) => Unit): Unit = {
  println("Executando sort")
  func(data.sortWith(_._2 > _._2), no_op)
}

// func: (Seq[(String, Int)], (Seq[(String, Int)], (() => Unit) => Unit) => Unit) => Unit
def frequencies(data: List[String], func: (Seq[(String, Int)], (Seq[(String, Int)], (() => Unit) => Unit) => Unit) => Unit): Unit = {
  println("Executando frequencies")
  func(data.groupBy(identity).mapValues(_.size).toSeq, print_text)
}

// func: (List[String], (Seq[(String, Int)], (Seq[(String, Int)], (() => Unit) => Unit) => Unit) => Unit) => Unit
def remove_stop_words(data: Array[String], func: (List[String], (Seq[(String, Int)], (Seq[(String, Int)], (() => Unit) => Unit) => Unit) => Unit) => Unit): Unit = {
  println("Executando remove_stop_words")
  var stop_words = scala.io.Source.fromFile("C:\\Users\\Felipe\\AppData\\Roaming\\JetBrains\\IdeaIC2020.3\\scratches\\WordCounter\\stop_words.txt").mkString
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

  func(new_data, sort)
}

// func: (Array[String], (List[String], (Seq[(String, Int)], (Seq[(String, Int)], (() => Unit) => Unit) => Unit) => Unit) => Unit) => Unit
def scan(data: String, func: (Array[String], (List[String], (Seq[(String, Int)], (Seq[(String, Int)], (() => Unit) => Unit) => Unit) => Unit) => Unit) => Unit): Unit = {
  println("Executando scan")
  val word_list = data.split(" +")
  func(word_list, frequencies)
}

// func: (String, (Array[String], (List[String], (Seq[(String, Int)], (Seq[(String, Int)], (() => Unit) => Unit) => Unit) => Unit) => Unit) => Unit) => Unit
def normalize(data: String, func: (String, (Array[String], (List[String], (Seq[(String, Int)], (Seq[(String, Int)], (() => Unit) => Unit) => Unit) => Unit) => Unit) => Unit) => Unit): Unit = {
  println("Executando normalize")
  func(data.toLowerCase(), remove_stop_words)
}

// func: (String, (String, (Array[String], (List[String], (Seq[(String, Int)], (Seq[(String, Int)], (() => Unit) => Unit) => Unit) => Unit) => Unit) => Unit) => Unit) => Unit
def filter_chars(data: String, func: (String, (String, (Array[String], (List[String], (Seq[(String, Int)], (Seq[(String, Int)], (() => Unit) => Unit) => Unit) => Unit) => Unit) => Unit) => Unit) => Unit): Unit = {
  println("Executando filter_chars")
  func(data.replaceAll("[^A-Za-z0-9 ]", " "), scan)
}

// func: (String, (String, (String, (Array[String], (List[String], (Seq[(String, Int)], (Seq[(String, Int)], (() => Unit) => Unit) => Unit) => Unit) => Unit) => Unit) => Unit) => Unit) => Unit
def read_file(path: String, func: (String, (String, (String, (Array[String], (List[String], (Seq[(String, Int)], (Seq[(String, Int)], (() => Unit) => Unit) => Unit) => Unit) => Unit) => Unit) => Unit) => Unit) => Unit): Unit = {
  println("Executando read_file")
  func(scala.io.Source.fromFile(path).mkString, normalize)
}

println("Iniciando o programa")
read_file("C:\\Users\\Felipe\\AppData\\Roaming\\JetBrains\\IdeaIC2020.3\\scratches\\WordCounter\\text.txt", filter_chars)
