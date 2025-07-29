##' Store and Encrypt R Objects or Files or Read and Decrypt Them
##'
##' `qcrypt` is used to protect sensitive information on a user's computer or when transmitting a copy of the file to another R user.  Unencrypted information only exists for a moment, and the encryption password does not appear in the user's script but instead is managed by the `keyring` package to remember the password across R sessions, and the `getPass` package, which pops up a password entry window and does not allow the password to be visible.  The password is requested only once, except perhaps when the user logs out of their operating system session or reboots.
##'
##' The keyring can be bypassed and the password entered in a popup window by specifying `service=NA`.  This is the preferred approach when sending an encrypted file to a user on a different computer.
##' 
##' `qcrypt` writes R objects to disk in a temporary file using the `qs` package `qsave` function.  The file is quickly encrypted using the `safer` package, and the temporary unencrypted `qs` file is deleted.  When reading an encrypted file the process is reversed.
##'
##' To save an object in an encrypted file, specify the object as the first argument `obj` and specify a base file name as a character string in the second argument `base`.  The full `qs` file name will be of the form `base.qs.encrypted` in the user's current working directory.  To unencrypt the file into a short-lived temporary file and use `qs::qread` to read it, specify the base file name as a character string with the first argument, and do not specify the `base` argument.
##'
##' Alternatively, `qcrypt` can be used to encrypt or decrypt existing files of any type using the same password and keyring mechanism.  The former is done by specifying `file` that does not end in `'.encrypted'` and the latter is done by ending `file` with `'.encrypted'`.  When `file` does not contain a path it is assumed to be in the current working directory.  When a file is encrypted the original file is removed.  Files are decrypted into a temporary directory created by `tempdir()`, with the name of the file being the value of `file` with `'.encrypted'` removed.
##'
##' Interactive password provision works when running `R`, `Rscript`, `RStudio`, or `Quarto` but does not work when running `R CMD BATCH`.  `getPass` fails under `RStudio` on Macs.
##'
##' It is also possible to pass the password as the `pw` argument.  This is only safe if running interactively and the password is defined by typing e.g. `pw <- 'whateverpassword'` in the console, then running the script interactively with `pw=pw` added to the `qcrypt` call.
##'
##' See [R Workflow](https://hbiostat.org/rflow/fcreate.html#sec-fcreate-secure) for more information.
##' @title qcrypt
##' @param obj an R object to write to disk and encrypt (if `base` is specified) or the base file name to read and uncrypted (if `base` is not specified).  Not used when `file` is given.
##' @param base base file name when creating a file.  Not used when `file` is given.
##' @param service a fairly arbitrary `keyring` service name.  The default is almost always OK unless you need to use different passwords for different files.  `service` is ignored if `pw` is specified as an argument.
##' @param file full name of file to encrypt or decrypt
##' @param pw a single character string containing an actual password
##' @return (invisibly) the full encrypted file name if writing the file, or the restored R object if reading the file.  When decrypting a general file with `file=`, the returned value is the full path to a temporary file containing the decrypted data.
##' @author Frank Harrell
##' @md
##' @export
##' @examples
##' \dontrun{
##' # Suppose x is a data.table or data.frame
##' # The first time qcrypt is run with a service a password will
##' # be requested.  It will be remembered across sessions thanks to
##' # the keyring package
##' qcrypt(x, 'x')   # creates x.qs.encrypted in current working directory
##' x <- qcrypt('x') # unencrypts x.qs.encrypted into a temporary
##'                  # directory, uses qs::qread to read it, and
##'                  # stores the result in x
##' # Encrypt a general file using a different password
##' qcrypt(file='report.pdf', service='pdfkey')
##' # Decrypt that file
##' fi <- qcrypt(file='report.pdf.encrypted', service='pdfkey')
##' fi contains the full unencrypted file name which is in a temporary directory
##' # Encrypt without using a keyring
##' qcrypt(x, 'x', service=NA)
##' x <- qcrypt('x', service=NA)
##'
##' pw <- 'somepassword'     # run this in the console
##' x <- qcrypt('x', pw=pw)  # interactively run this in a script
##' }
qcrypt <- function(obj, base, service='R-keyring-service', file, pw) {
  if(missing(pw)) {
    if(! is.na(service) && ! requireNamespace('keyring', quietly=TRUE))
      stop('you must install the keyring package to use qcrypt with service != NA')
    if(! requireNamespace('getPass', quietly=TRUE))
      stop('you must install the getPass package to use qcrypt')
    if(! requireNamespace('qs', quietly=TRUE))
      stop('you must install the qs package to use qcrypt')
    if(! requireNamespace('safer', quietly=TRUE))
      stop('you must install the safer package to use qcrypt')

    if(is.na(service)) {
      prompt <- if(! missing(base)) 'Define password for storing encrypted data: '
      else 'Enter password previously used to store data: '
      pw <- getPass::getPass(msg = prompt, noblank=TRUE)
      } else {
      pw <- tryCatch(keyring::key_get(service), error = function(...) '')

      if(pw == '') {
        prompt <- if(! missing(base)) 'Define password for storing encrypted data: '
        else 'Enter password previously used to store data: '
        pw <- getPass::getPass(msg = prompt, noblank=TRUE)
        keyring::key_set_with_value(service, password=pw)
      }
      pw <- keyring::key_get(service)
    }
  }
  tf <- tempfile()

  if(! missing(file)) {
    if(! grepl('\\.encrypted$', file)) {  # encrypt
      f <- paste0(file, '.encrypted')
      unlink(f)   # just in case; encrypt_file throws an error if file exists
      safer::encrypt_file(file, key=pw, outfile=f)
      unlink(file)
      return(invisible(f))
    } else { # decrypt
      fout <- paste(tempdir(), sub('\\.encrypted$', '', basename(file)), sep='/')
      safer::decrypt_file(file, key=pw, outfile=fout)
      return(invisible(fout))
    }
  }

  if(! missing(base)) {  # base specified -> create file
    qs::qsave(obj, tf)
    f <- paste0(base, '.qs.encrypted')
    unlink(f)    # just in case
    safer::encrypt_file(tf, key=pw, outfile=f)
    unlink(tf)   # quickly remove unencrypted file
    return(invisible(f))
  }

  # base is missing and base file name is assumed to be in the first argument, obs

  f <- paste0(obj, '.qs.encrypted')
  safer::decrypt_file(f, key=pw, outfile=tf)
  x <- qs::qread(tf)
  unlink(tf)    # quickly remove unencrypted file
  x 
}
