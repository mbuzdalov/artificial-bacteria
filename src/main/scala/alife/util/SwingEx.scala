package alife.util

import javax.swing.SwingUtilities

/**
 * Various useful Swing-related function adapters that make them more Scala-friendly. 
 */
object SwingEx:
  /**
   * Invokes the given body later in the Swing thread. Wraps `SwingUtilities.invokeLater`.
   * @param body the body to invoke
   */
  inline def invokeLater(inline body: => Any): Unit =
    SwingUtilities.invokeLater(() => body)
  
  /**
   * Invokes the given body in the Swing thread and block until it succeeds. Wraps `SwingUtilities.invokeAndWait`.
   * @param body the body to invoke.
   */
  inline def invokeAndWait(inline body: => Any): Unit =
    SwingUtilities.invokeAndWait(() => body)
    