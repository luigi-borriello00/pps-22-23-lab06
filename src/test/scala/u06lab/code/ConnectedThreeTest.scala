package u06lab.code

import org.junit.Test
import org.junit.Assert.*
import ConnectThree.Player.*
import ConnectThree.*

class ConnectedThreeTest :

  @Test
  def testFind() =
    assertEquals(Some(X), find(List(Disk(0, 0, X)), 0, 0))
    assertEquals(Some(O), find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 0, 1))
    assertEquals(None, find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 1, 1))

  @Test
  def testFindFirstAvailableRow() =
    assertEquals(Some(0), firstAvailableRow(List(), 0)) // Some(0)
    assertEquals(Some(1), firstAvailableRow(List(Disk(0, 0, X)), 0)) // Some(1)
    assertEquals(Some(2), firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X)), 0)) // Some(2)
    assertEquals(Some(3), firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X)), 0)) // Some(3)
    assertEquals(None, firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X), Disk(0, 3, X)), 0)) // None
