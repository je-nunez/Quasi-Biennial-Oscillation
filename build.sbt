
name := "QsBOsc"

version := "0.0.1"

scalaVersion := "2.11.8"

scalacOptions ++= Seq("-unchecked", "-deprecation")

fork in run := true
fork in Test := true

javaOptions in run ++= Seq(
    "-Xms4G", "-Xmx4G", "-XX:+UseG1GC"
)

// remove the [info] preffixes given by SBT
outputStrategy        :=   Some(StdoutOutput)


testOptions in Test += Tests.Argument("-oD")

concurrentRestrictions in Global += Tags.limit(Tags.Test, 1)

val versions = Map(
                    "spark"    -> "1.6.2",
                    "sparkts"    -> "0.4.0",    // Time Series Analysis for Spark
                    "deeplearning4j"    -> "0.6.0",
                    "nd4j" -> "0.6.0",
                    "gephi"    -> "0.9.1",
                    "jTransforms"    -> "3.1",   // for FFT
                    "twelvemonkeys"    -> "3.2.1",    // ImageIO
                    "gral-core"    -> "0.11",    // plotting
                    "scalaArm" -> "1.4",
                    "jackson-databind" -> "2.4.4"
                  )

val gephiToolkitURL = "https://github.com/gephi/gephi-toolkit/releases/download/v0.9.1/gephi-toolkit-0.9.1-all.jar"

libraryDependencies ++= Seq(
  "org.apache.spark" %% "spark-core" % versions("spark") % "provided",
  "org.apache.spark" %% "spark-mllib" % versions("spark"),
  "com.cloudera.sparkts" % "sparkts" % versions("sparkts"),
  "org.deeplearning4j" % "deeplearning4j-core" % versions("deeplearning4j"),
  "org.deeplearning4j" % "deeplearning4j-nn" % versions("deeplearning4j"),
  "org.nd4j" % "nd4j-native-platform" % versions("nd4j"),
  "org.gephi" % "gephi-toolkit" % versions("gephi") from gephiToolkitURL,
  "com.github.wendykierp" % "JTransforms" % versions("jTransforms"),

  "com.twelvemonkeys.common" % "common-lang" % versions("twelvemonkeys"),
  "com.twelvemonkeys.common" % "common-io" % versions("twelvemonkeys"),
  "com.twelvemonkeys.common" % "common-image" % versions("twelvemonkeys"),
  "com.twelvemonkeys.imageio" % "imageio-core" % versions("twelvemonkeys"),
  "com.twelvemonkeys.imageio" % "imageio-metadata" % versions("twelvemonkeys"),
  "com.twelvemonkeys.imageio" % "imageio-jpeg" % versions("twelvemonkeys"),
  "com.twelvemonkeys.imageio" % "imageio-tiff" % versions("twelvemonkeys"),

  "de.erichseifert.gral" % "gral-core" % versions("gral-core"),
  "org.apache.commons" % "commons-lang3" % "3.0",
  "com.jsuereth" %% "scala-arm" % versions("scalaArm"),
  "org.scalatest" %% "scalatest" % "2.2.6" % "test"
)

dependencyOverrides ++= Set(
  "com.fasterxml.jackson.core" % "jackson-databind" % versions("jackson-databind")
)

resolvers ++= Seq(
  "Netbeans Repository" at "http://bits.netbeans.org/nexus/content/groups/netbeans/",
  "JBoss Repository" at "http://repository.jboss.org/nexus/content/repositories/releases/",
  "Spray Repository" at "http://repo.spray.cc/",
  "Cloudera Repository" at "https://repository.cloudera.com/artifactory/cloudera-repos/",
  "Akka Repository" at "http://repo.akka.io/releases/",
  "Twitter4J Repository" at "http://twitter4j.org/maven2/",
  "Apache HBase" at "https://repository.apache.org/content/repositories/releases",
  "Twitter Maven Repo" at "http://maven.twttr.com/",
  "scala-tools" at "https://oss.sonatype.org/content/groups/scala-tools",
  "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
  "Second Typesafe repo" at "http://repo.typesafe.com/typesafe/maven-releases/",
  "Mesosphere Public Repository" at "http://downloads.mesosphere.io/maven",
  Resolver.sonatypeRepo("public")
)

