package edu.vtc.nessie

import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream

object FileManager {

  /**
   * Remove a folder and all its contents recursively. This method attempts to delete the
   * specified folder as well as everything in it.
   *
   * @param folderName The name of the folder to delete.
   * @return True of the deletion was successful; false otherwise.
   */
  def deleteFolder(folderName: File): Boolean = {
    var returnCode: Boolean = true
    val containedNames = folderName.list()
    for (name <- containedNames) {
        val entity = new File(folderName, name)
        if (entity.isFile) {
            if (!entity.delete()) returnCode = false
        }
        else if (entity.isDirectory) {
            if (!deleteFolder(entity)) returnCode = false
        }
        else {
            returnCode = false
        }
    }
    if (!folderName.delete()) returnCode = false
    returnCode
  }


  // This method needs to be improved. It isn't very exception safe at the moment.
  def copyFile(source: File, destination: File): Unit = {
    val buffer = new Array[Byte](1024)
    val input = new FileInputStream(source)
    val output = new FileOutputStream(destination)
    var count: Int = 0

    while ( { count = input.read(buffer); count != -1 } ) {
      output.write(buffer, 0, count)
    }

    input.close()
    output.close()
  }
}
