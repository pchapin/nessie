package edu.vtc.nessie

import java.io.File
import java.io.FilenameFilter

/**
 * Select only nesC files. This class filters a list of file names selecting only those that
 * end with *.nc. Note that *.h files are not selected because they are merged into the nesC
 * files by the preprocessor.
 * 
 * @author Peter
 */
class nesCFilter extends FilenameFilter {

  def accept(dir: File, name: String) = {
    if (name.endsWith(".nc")) true else false
  }
}
