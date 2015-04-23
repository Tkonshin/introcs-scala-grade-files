import scala.util.{ Try, Success, Failure }
import scala.math.min 
import scala.io._
import scala.util.Sorting
import java.io._
import scala.util._
import scala.io.Source

object Homework23 extends App {
    
    println("Course Name?")
    val courseName = Try(args(0)) getOrElse(readLine())
    val writer = new PrintWriter (new File(courseName + "_solution.txt"))
    val courseFileName = s"categories_$courseName.txt"
    println(s">> Reading $courseName categories file")
    val source = Source.fromFile(courseFileName)
    val sourceLines = source.getLines.toArray
    val categoriesArray:Array[String] = (sourceLines(0)).split(", ")
    val categories = trim(categoriesArray.mkString)
    val weightsArray = sourceLines(1)
    val weights = trim(weightsArray.mkString)
    val numberGrades = trim((sourceLines(2)).mkString)
    val columns = weights.length
    var totalWeight = 0
    for (y <- 0 until columns){
        totalWeight = totalWeight + weightsArray(y).toInt
    }
    var ClassParticipationweight = 0
    var Examsweight = 0
    var Homeworksweight = 0
    var Labsweight = 0
    var Projectsweight = 0
    for(y <- 0 until columns){
        if(categoriesArray(y).startsWith("C")){
            ClassParticipationweight += weightsArray(y).toInt
        }
        else if(categoriesArray(y).startsWith("E")){
            Examsweight += weightsArray(y).toInt
        }
        else if(categoriesArray(y).startsWith("H")){
            Homeworksweight += weightsArray(y).toInt
        }
        else if(categoriesArray(y).startsWith("L")){
            Labsweight += weightsArray(y).toInt
        }
        else if(categoriesArray(y).startsWith("P")){
            Projectsweight += weightsArray(y).toInt
        }
    }
    var ClassParticipationWeightT = (ClassParticipationweight.toDouble)/(totalWeight.toDouble)
    var ExamsWeightT = (Examsweight.toDouble)/(totalWeight.toDouble)
    var HomeworksWeightT = (Homeworksweight.toDouble)/(totalWeight.toDouble)
    var LabsWeightT = (Labsweight.toDouble)/(totalWeight.toDouble)
    var ProjectsWeightT = (Projectsweight.toDouble)/(totalWeight.toDouble)
    
    println(s">> Reading $courseName student file")
    readClassStudentFile(courseName)
    
    def trim (line: String) : Array[String] = {
        var lineSplit = line.split(",")
        for (y <- 0 until lineSplit.length){
            lineSplit(y) = lineSplit(y).trim
        }
        (lineSplit)
    }   
    def readClassStudentFile (courseName : String) : String = {
        val studentClassFileName = s"students_$courseName.txt"
        val studentClassFile = Source.fromFile(studentClassFileName)
        val files = studentClassFile.getLines
        while (files.hasNext){
            files.foreach(line => studentBufferLine(line))
            }       
        ""
    }
        
     def studentBufferLine (line : String) : (String) = {
        val studentline = line.split(", ")
        val studentIDnumber = studentline(0)
        val studentLastName = studentline(1)
        val studentFirstName = studentline(2)
        //println(">> Obtaining file information for: " + studentFirstName + " " + studentLastName)
        var iAC = studentFile(studentIDnumber, courseName)
        writer.write(studentLastName + ", " + studentFirstName + " " + iAC + "\n")
        ""
     }
     
    def studentFile (studentIDnumber : String, courseName : String) : String = {
        val studentFileName = studentIDnumber + courseName + ".data"
        val studentClassFile = Source.fromFile(studentFileName)
        val storage:Array[String] = studentClassFile.getLines.toArray
        var ClassParticipationy = 0
        var Examsy = 0
        var Homeworksy = 0
        var Labsy = 0
        var Projectsy = 0
        var ClassParticipationArray = new Array[String](37)
        var ExamsArray = new Array[String](37)
        var HomeworksArray = new Array[String](37)
        var LabsArray = new Array[String](37)
        var ProjectsArray = new Array[String](37)
        for (y <- 0 until storage.size){
            if (storage(y).startsWith("C")){
                ClassParticipationArray(ClassParticipationy) = (storage(y) + ", ") 
                ClassParticipationy = ClassParticipationy + 1
            }
            else if (storage(y).startsWith("E")){
                ExamsArray(Examsy) = storage(y) + ", "
                Examsy = Examsy + 1
            }
            else if (storage(y).startsWith("H")){
                HomeworksArray(Homeworksy) = storage(y) + ", "
                Homeworksy = Homeworksy + 1
            }             
            else if (storage(y).startsWith("L")){
                LabsArray(Labsy) = storage(y) + ", "
                Labsy = Labsy + 1
            }
            else if (storage(y).startsWith("P")){
                ProjectsArray(Projectsy) = storage(y) + ", "
                Projectsy = Projectsy + 1
            }
        }        
        var B = ClassParticipationArray.slice(0, ClassParticipationy)
        var E = ExamsArray.slice(0, Examsy)
        var D = HomeworksArray.slice(0, Homeworksy)
        var A = LabsArray.slice(0, Labsy)
        var C = ProjectsArray.slice(0, Projectsy)        
        def trimmedValues (line : Array[String]) : Array[String] = {
            var trimmedValues = (line.mkString).split(", ")
            for (y <- 0 until trimmedValues.length){
                trimmedValues(y) = trimmedValues(y).trim
            }
            trimmedValues
        }                
        var ClassParticipationTrimmed = trimmedValues(B)
        var ExamsTrimmed = trimmedValues(E)
        var HomeworksTrimmed = trimmedValues(D)
        var LabsTrimmed = trimmedValues(A)
        var ProjectsTrimmed = trimmedValues(C)        
        def calcSum (line: Array[String]) : Int = {
            var distance = line.length
            var sum = 0
            for (y <- 2 until distance by 3){
                sum = sum + line(y).toInt
            }
            sum
        }        
        var ClassParticipationSum = calcSum(ClassParticipationTrimmed)
        var ExamsSum = calcSum(ExamsTrimmed)
        var HomeworksSum = calcSum(HomeworksTrimmed)
        var LabsSum = calcSum(LabsTrimmed)
        var ProjectsSum = calcSum(ProjectsTrimmed)       
        var ClassParticipationAvg:Double = 0
        var ExamsAvg:Double = 0
        var HomeworksAvg:Double = 0
        var LabsAvg:Double = 0
        var ProjectsAvg:Double = 0
        if (ClassParticipationy != 0){
            ClassParticipationAvg = (ClassParticipationSum.toDouble)/(ClassParticipationy.toDouble)
        } 
        if (Examsy != 0){
            ExamsAvg = (ExamsSum.toDouble)/(Examsy.toDouble)
        } 
        if (Homeworksy != 0){
            HomeworksAvg = (HomeworksSum.toDouble)/(Homeworksy.toDouble)
        } 
        if (Labsy != 0){
            LabsAvg = (LabsSum.toDouble)/(Labsy.toDouble)
        } 
        if (Projectsy != 0){
            ProjectsAvg = (ProjectsSum.toDouble)/(Projectsy.toDouble)
        }
        var ClassParticipationC = (ClassParticipationAvg)*(ClassParticipationWeightT)
        var examEnd = (ExamsAvg)*(ExamsWeightT)
        var labEnd = (LabsAvg)*(LabsWeightT)
        var homeworkEnd = (HomeworksAvg)*(HomeworksWeightT)
        var projectEnd = (ProjectsAvg)*(ProjectsWeightT)       
        var Answer:String = ""       
        var numberGrade = (ClassParticipationC + examEnd + labEnd + homeworkEnd + projectEnd)
        var semesterGrade = f"$numberGrade%2.1f"       
        val gradingScale = Array(93, 90, 87, 83, 80, 73, 70, 67, 60, 0)
        val gradingScaleLetter = Array("A", "A-", "B+", "B", "B-", "C", "C-", "D+", "D", "F")
        var i = 0
        var k = 0
        while(i != 1){
            if(numberGrade >= gradingScale(k)){
                Answer = semesterGrade + gradingScaleLetter(k)
                i +=1
            }
            k += 1
        }
        Answer
    }
    writer.close
}