package org.enricobn.shell.impl

import org.enricobn.shell.CommandHistoryStore
import org.enricobn.vfs.IOError

/**
  * Created by enrico on 12/10/16.
  */
class CommandHistory(private val store: CommandHistoryStore, private val maxSize: Int = 100) {
  private var current = 0
  private var currentLine: Option[String] = None

  def prev(currentLine: String): Either[IOError, Option[String]] =
    store.length.right.flatMap { l =>
      if (current == l) {
        this.currentLine = Some(currentLine)
      }
      val next = current - 1
      if (next >= 0) {
        current = next
        store(next).right.map(Some(_))
      } else {
        Right(None)
      }
    }

  def succ() : Either[IOError, Option[String]] =
    store.length.right.flatMap { l =>
      val next = current + 1
      if (next < l) {
        current = next
        store(next).right.map(Some(_))
      } else {
        current = l
        Right(currentLine)
      }
    }

  def add(command: String) : Either[IOError, Unit] = {
    store.lastOption.right.flatMap { lo =>
      val addCommandE =
        if (!lo.contains(command)) {
          store.add(command).right.flatMap { _ =>
            store.length.right.flatMap { l =>
              if (l > maxSize) {
                store.removeHead
              } else {
                Right(())
              }
            }
          }
        } else {
          Right(())
        }

      addCommandE.right.flatMap { _ =>
        store.length.right.map { l =>
          current = l
          currentLine = None
          ()
        }
      }
    }
  }

}
