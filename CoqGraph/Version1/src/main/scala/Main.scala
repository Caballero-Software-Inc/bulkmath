import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.functions.{udf, col}

import scala.util.matching.Regex

object Main extends App {

  val conf = new SparkConf()
        .setAppName("CoqGraphApp")
        .setMaster("local[*]")

  val sc = new SparkContext(conf)
 
  val spark = SparkSession.builder.config(sc.getConf).getOrCreate()
  import spark.implicits._

  val df = spark.read.option("delimiter", ",")
    .option("header", "true")
    .csv("/home/josephcmac/Documents/GitHub/bulkmath/Datasets/Dataset1/")
    .na.drop()

  val names = df.select("name")
    .rdd.map(r => r(0).toString).collect().toList

  val extractNamesUDF = udf( 
    (s: String) => 
      names.filter(name => (s contains ("." + name + ".")))
  )
  
  val df2 = df.select(col("name"), extractNamesUDF(col("proof")).as("mentions"), col("address"))

  df2.repartition(1).write.parquet("Output")

  spark.stop()    

}