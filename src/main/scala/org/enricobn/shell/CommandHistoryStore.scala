package org.enricobn.shell

import org.enricobn.vfs.IOError

trait CommandHistoryStore {

    def add(command: String) : Either[IOError, Unit]

    def apply(i: Int) : Either[IOError, String]

    def length : Either[IOError, Int]

    def lastOption : Either[IOError, Option[String]]

    def removeHead : Either[IOError, Unit]

}
