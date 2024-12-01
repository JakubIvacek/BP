import java.lang
import scala.collection.mutable

object ModuleManager {
  // A collection to hold the modules (using a mutable map for easy management)
  private val modules: mutable.Map[String, Module] = mutable.Map()

  def addModule(module: Module): Unit = {
    if (modules.contains(module.name)) {
      println(s"Module with name '${module.name}' already exists.")
    } else {
      module.installModuleData()
      modules.put(module.name, module)
      println(s"Module '${module.name}' added.")
    }
  }

  def removeModule(name: String): Unit = {
    val module = findModule(name)
    module match {
      case Some(m) =>
        println(s"Module '$name' found. Removing associated data...")
        m.removeModuleData()
        modules.remove(name)
        println(s"Module '$name' has been removed from the manager.")
      case None =>
        println(s"No module found with name '$name'.")
    }
  }

  def listModules(): Unit = {
    if (modules.isEmpty) {
      println("No modules available.")
    } else {
      println("Modules:")
      modules.values.foreach(_.displayDetails())
    }
  }

  def totalModulesSize(): Long = {
    val totalSize = modules.values.map(_.moduleSize()).sum
    println("Total size of all modules: " + totalSize + " bytes")
    totalSize
  }
  private def findModule(name: String): Option[Module] = {
    modules.get(name)
  }
}

