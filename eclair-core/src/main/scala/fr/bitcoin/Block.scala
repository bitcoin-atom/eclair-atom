package fr.acinq.bitcoin

import java.io.{ByteArrayOutputStream, InputStream, OutputStream}
import java.util

import fr.acinq.bitcoin.Protocol._

object BlockHeader extends BtcSerializer[BlockHeader] {
  override def read(input: InputStream, protocolVersion: Long): BlockHeader = {
    val version = uint32(input)
    val hashPreviousBlock = hash(input)
    val hashMerkleRoot = hash(input)
    val time = uint32(input)
    val bits = uint32(input)
    val nonce = uint32(input)
    val flags = uint32(input)
    BlockHeader(version, hashPreviousBlock, hashMerkleRoot, time, bits, nonce, flags)
  }

  override def write(input: BlockHeader, out: OutputStream, protocolVersion: Long): Unit = {
    writeBase(input, out, protocolVersion)
    writeUInt32(input.flags.toInt, out)
  }

  def writeBase(input: BlockHeader, out: OutputStream, protocolVersion: Long): Unit = {
    writeUInt32(input.version.toInt, out)
    writeBytes(input.hashPreviousBlock, out)
    writeBytes(input.hashMerkleRoot, out)
    writeUInt32(input.time.toInt, out)
    writeUInt32(input.bits.toInt, out)
    writeUInt32(input.nonce.toInt, out)
  }
}

/**
  *
  * @param version           Block version information, based upon the software version creating this block
  * @param hashPreviousBlock The hash value of the previous block this particular block references. Please not that
  *                          this hash is not reversed (as opposed to Block.hash)
  * @param hashMerkleRoot    The reference to a Merkle tree collection which is a hash of all transactions related to this block
  * @param time              A timestamp recording when this block was created (Will overflow in 2106[2])
  * @param bits              The calculated difficulty target being used for this block
  * @param nonce             The nonce used to generate this block… to allow variations of the header and compute different hashes
  */
case class BlockHeader(version: Long, hashPreviousBlock: BinaryData, hashMerkleRoot: BinaryData,
                       time: Long, bits: Long, nonce: Long, flags: Long) extends BtcSerializable[BlockHeader] {

  require(hashPreviousBlock.length == 32, "hashPreviousBlock must be 32 bytes")
  require(hashMerkleRoot.length == 32, "hashMerkleRoot must be 32 bytes")

  lazy val hash: BinaryData =
    if (isProofOfStake) {
      val out = new ByteArrayOutputStream()
      BlockHeader.writeBase(this, out, PROTOCOL_VERSION)
      out.write((0xFF & flags).toInt)
      out.write(0)
      out.write(0)
      out.write(0)

      Crypto.hash256(out.toByteArray)
    } else {
      val out = new ByteArrayOutputStream()
      BlockHeader.writeBase(this, out, PROTOCOL_VERSION)

      Crypto.hash256(BinaryData(out.toByteArray))
    }

  // hash is reversed here (same as tx id)
  lazy val blockId = BinaryData(hash.reverse)

  override def serializer: BtcSerializer[BlockHeader] = BlockHeader

  def isProofOfStake: Boolean = (flags & 1) == 1
}

object Block extends BtcSerializer[Block] {
  override def read(input: InputStream, protocolVersion: Long): Block = {
    val raw = bytes(input, 80)
    val header = BlockHeader.read(raw)
    Block(header, readCollection[Transaction](input, protocolVersion))
  }

  override def write(input: Block, out: OutputStream, protocolVersion: Long) = {
    BlockHeader.write(input.header, out)
    writeCollection(input.tx, out, protocolVersion)
  }

  override def validate(input: Block): Unit = {
    BlockHeader.validate(input.header)
    require(util.Arrays.equals(input.header.hashMerkleRoot, MerkleTree.computeRoot(input.tx.map(_.hash))), "invalid block:  merkle root mismatch")
    require(input.tx.map(_.txid).toSet.size == input.tx.size, "invalid block: duplicate transactions")
    input.tx.foreach(Transaction.validate)
  }

  // genesis blocks
  val LivenetGenesisBlock = {
    val script = OP_PUSHDATA(writeUInt32(486604799L)) :: OP_PUSHDATA(BinaryData("04")) :: OP_PUSHDATA("The Times 03/Jan/2009 Chancellor on brink of second bailout for banks".getBytes("UTF-8")) :: Nil
    val scriptPubKey = OP_PUSHDATA("04678afdb0fe5548271967f1a67130b7105cd6a828e03909a67962e0ea1f61deb649f6bc3f4cef38c4f35504e51ec112de5c384df7ba0b8d578a4c702b6bf11d5f") :: OP_CHECKSIG :: Nil
    Block(
      BlockHeader(version = 1, hashPreviousBlock = Hash.Zeroes, hashMerkleRoot = "3ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a", time = 1231006505, bits = 0x1d00ffff, nonce = 2083236893, flags = 0L),
      List(
        Transaction(version = 1,
          txIn = List(TxIn.coinbase(script)),
          txOut = List(TxOut(amount = 50 btc, publicKeyScript = scriptPubKey)),
          lockTime = 0))
    )
  }

  val TestnetGenesisBlock = LivenetGenesisBlock.copy(header = LivenetGenesisBlock.header.copy(time = 1296688602, nonce = 414098458))

  val RegtestGenesisBlock = LivenetGenesisBlock.copy(header = LivenetGenesisBlock.header.copy(bits = 0x207fffffL, nonce = 2, time = 1296688602))

  val SegnetGenesisBlock = LivenetGenesisBlock.copy(header = LivenetGenesisBlock.header.copy(bits = 503447551, time = 1452831101, nonce = 0))

  val BCATestnetForkBlockHash = BinaryData("00000099df3f94c286e781f00810923653de9363454ddf55c6957c0d2bb085f0")

  val BCARegtestForkBlockHash = BinaryData("25e97734d280bbb9cde52e7daa9d539f21d556b81a0c7dd43b6930bf857a412d")

  val BCALivenetForkBlockHash = BinaryData("0000015bff8479f82eab3463a9d1a65184127893359324322ce3ffa6e93d16db")
}

/**
  * Bitcoin block
  *
  * @param header block header
  * @param tx     transactions
  */
case class Block(header: BlockHeader, tx: Seq[Transaction]) extends BtcSerializable[Block] {
  lazy val hash = header.hash

  lazy val blockId = header.blockId

  override def serializer: BtcSerializer[Block] = Block
}

