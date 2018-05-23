package fr.acinq.bitcoin

import java.io.ByteArrayInputStream

import com.google.common.base.Charsets
import com.google.common.io.Resources
import fr.acinq.bitcoin.Protocol.PROTOCOL_VERSION
import fr.acinq.eclair.blockchain.WatcherSpec
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
  * Created by anton on 14.05.18.
  */
@RunWith(classOf[JUnitRunner])
class BlockSpec extends FunSuite {

  test("block hash") {
    {
      val resource = classOf[WatcherSpec].getResource("/block1325549_testnet.txt")
      val text = Resources.toString(resource, Charsets.UTF_8)
      val block1325549TestPoS = new ByteArrayInputStream(BinaryData(text))
      val header = BlockHeader.read(block1325549TestPoS, PROTOCOL_VERSION)

      assert(BinaryData("8e3aae1d7d8de407283a60e2123d3bbaa3aab7c525a1bd2537e166ee23d68047") == header.blockId)
    }

    {
      val block1300000TestPoW = new ByteArrayInputStream(BinaryData("0000002035742815054d72d3a563f87eb5bb6cbf387fa64b9ebebdfdbb4039346607000" +
        "08565906b46214aceb4546954fa9fc08ecd79a077fb50faf197a0d5b887a26c8893d5795affff071e80034bb900000080010100000000010100000" +
        "00000000000000000000000000000000000000000000000000000000000ffffffff040320d613ffffffff02c817a804000000001976a91417c5363" +
        "74baffa9178293e8e8b28fb43ac7e9a7488ac0000000000000000266a24aa21a9ede2f61c3f71d1defd3fa999dfa36953755c690689799962b48be" +
        "bd836974e8cf90120000000000000000000000000000000000000000000000000000000000000000000000000"))

      val header = BlockHeader.read(block1300000TestPoW, PROTOCOL_VERSION)

      assert(BinaryData("0000011c90b36f9d568bdf9289de72b074ab1a9ccc323fbc9bbff9dd85e4b18c") == header.blockId)
    }

    assert(BinaryData("000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f") == Block.LivenetGenesisBlock.blockId)
    assert(BinaryData("000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943") == Block.TestnetGenesisBlock.blockId)
  }

}