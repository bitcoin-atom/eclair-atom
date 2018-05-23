/*
 * Copyright 2018 ACINQ SAS
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package fr.acinq.eclair.payment

import java.nio.ByteOrder

import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}
import fr.acinq.bitcoin.{Bech32, BinaryData, Block, Btc, Crypto, MilliBtc, MilliSatoshi, Protocol, Satoshi}
import fr.acinq.eclair.ShortChannelId
import fr.acinq.eclair.payment.PaymentRequest._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
  * Created by fabrice on 15/05/17.
  */
@RunWith(classOf[JUnitRunner])
class PaymentRequestSpec extends FunSuite {

  val priv = PrivateKey(BinaryData("e126f68f7eafcc8b74f54d269fe206be715000f94dac067d1c04a8ca3b2db734"), compressed = true)
  val pub = priv.publicKey
  val nodeId = pub
  assert(nodeId == PublicKey(BinaryData("03e7156ae33b0a208d0744199163177e909e80176e55d97a2f221ede0f934dd9ad")))

  test("check minimal unit is used") {
    assert('p' === Amount.unit(MilliSatoshi(1)))
    assert('p' === Amount.unit(MilliSatoshi(99)))
    assert('n' === Amount.unit(MilliSatoshi(100)))
    assert('p' === Amount.unit(MilliSatoshi(101)))
    assert('n' === Amount.unit(Satoshi(1)))
    assert('u' === Amount.unit(Satoshi(100)))
    assert('n' === Amount.unit(Satoshi(101)))
    assert('u' === Amount.unit(Satoshi(1155400)))
    assert('m' === Amount.unit(MilliBtc(1)))
    assert('m' === Amount.unit(MilliBtc(10)))
    assert('m' === Amount.unit(Btc(1)))
  }

  test("check that we can still decode non-minimal amount encoding") {
    assert(Some(MilliSatoshi(100000000)) == Amount.decode("1000u"))
    assert(Some(MilliSatoshi(100000000)) == Amount.decode("1000000n"))
    assert(Some(MilliSatoshi(100000000)) == Amount.decode("1000000000p"))
  }

  test("Please make a donation of any amount using payment_hash 0001020304050607080900010203040506070809000102030405060708090102 to me @03e7156ae33b0a208d0744199163177e909e80176e55d97a2f221ede0f934dd9ad") {
    val ref = "lnbca1pdw3gtxpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdpl2pkx2ctnv5sxxmmwwd5kgetjypeh2ursdae8g6twvus8g6rfwvs8qun0dfjkxaqru9gnlrrvancfg5thgvkm63cac0rrvgrlrg3kq0q7e70l0vkaymjh567kr5658up4wnvgqc23shpykjvzf850kxl04p7jzksg57zvcqpklufl8"
    val pr = PaymentRequest.read(ref)
    assert(pr.prefix == "lnbca")
    assert(pr.amount.isEmpty)
    assert(pr.paymentHash == BinaryData("0001020304050607080900010203040506070809000102030405060708090102"))
    assert(pr.timestamp == 1525195110L)
    assert(pr.nodeId == PublicKey(BinaryData("03e7156ae33b0a208d0744199163177e909e80176e55d97a2f221ede0f934dd9ad")))
    assert(pr.description == Left("Please consider supporting this project"))
    assert(pr.fallbackAddress === None)
    assert(pr.tags.size === 2)
    assert(PaymentRequest.write(pr.sign(priv)) == ref)
  }

  test("Please send $3 for a cup of coffee to the same peer, within 1 minute") {
    val ref = "lnbca2500u1pdwjmwgpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdq5xysxxatsyp3k7enxv4jsfzme726098qkvw2dzxcqxp6wtde5cx3dzt34q5m6fnpc6fgazh094r4x6chh3gtmdwfsww2lf8s5ydzulwysgqkg0d3y2a64vepew5squwyrgl"
    val pr = PaymentRequest.read(ref)
    assert(pr.prefix == "lnbca")
    assert(pr.amount == Some(MilliSatoshi(250000000L)))
    assert(pr.paymentHash == BinaryData("0001020304050607080900010203040506070809000102030405060708090102"))
    assert(pr.timestamp == 1525247432L)
    assert(pr.nodeId == PublicKey(BinaryData("03e7156ae33b0a208d0744199163177e909e80176e55d97a2f221ede0f934dd9ad")))
    assert(pr.description == Left("1 cup coffee"))
    assert(pr.fallbackAddress === None)
    assert(pr.tags.size === 2)
    assert(PaymentRequest.write(pr.sign(priv)) == ref)
  }

  test("The same, on testnet, with a fallback address 2My3DfZgsrAirUsf89rdc631TLqdB6vxGSj") {
    val ref = "lntbca20m1pdwjatfpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdzafahx2grsd9jkxefqdanzqcmgda3k7mrpw3jjqcmpddjjcgr0dejjq6trv43hyetpd5sxxmmwv5kzqmmwv5s8q6trddkx2fppj879hue0yrdj3qwacc0teknxp5w4p59lysl04ddc2jpg57aulhj44arxf8slvfz9q0u79yglxxn4u24upr0xj2r00ne93wtqq70awwq0r93r5dtumcvp3wuh63v3jlcp5pje0flcpm8yk49"
    val pr = PaymentRequest.read(ref)
    assert(pr.prefix == "lntbca")
    assert(pr.amount == Some(MilliSatoshi(2000000000L)))
    assert(pr.paymentHash == BinaryData("0001020304050607080900010203040506070809000102030405060708090102"))
    assert(pr.timestamp == 1525249385L)
    assert(pr.nodeId == PublicKey(BinaryData("03e7156ae33b0a208d0744199163177e909e80176e55d97a2f221ede0f934dd9ad")))
    assert(pr.description == Left("One piece of chocolate cake, one icecream cone, one pickle"))
    assert(pr.fallbackAddress === Some("2My3DfZgsrAirUsf89rdc631TLqdB6vxGSj"))
    assert(pr.tags.size == 3)
    assert(PaymentRequest.write(pr.sign(priv)) == ref)
  }

  test("On mainnet, with fallback address 2My3DfZgsrAirUsf89rdc631TLqdB6vxGSj with extra routing info to go via nodes 029e03a901b85534ff1e92c43c74431f7ce72046060fcf7a95c37e148f78c77255 then 039e03a901b85534ff1e92c43c74431f7ce72046060fcf7a95c37e148f78c77255") {
    val ref = "lnbca20m1pdwj708pp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdzafahx2grsd9jkxefqdanzqcmgda3k7mrpw3jjqcmpddjjcgr0dejjq6trv43hyetpd5sxxmmwv5kzqmmwv5s8q6trddkx2fppj879hue0yrdj3qwacc0teknxp5w4p59lyr9yq20q82gphp2nflc7jtzrcazrra7wwgzxqc8u7754cdlpfrmccae92qqqqqqqqqqqqyqqqqq2qqqqqzcqpspeuqafqxu92d8lr6fvg0r5gv0heeeqgcrqlnm6jhphu9y00rrhy4gqqqqqqqqqqqpqqqqqpgqqqqqtqqxq0xsd753tkksaqxw4r8z6lsngn2s5x68sxgd6pmxcctxjcf69vef4axeauz5qggp39s5afyruj7fny3nvtxqwv85n42acydpwgeeszkcq99udl8"
    val pr = PaymentRequest.read(ref)
    assert(pr.prefix == "lnbca")
    assert(pr.amount === Some(MilliSatoshi(2000000000L)))
    assert(pr.paymentHash == BinaryData("0001020304050607080900010203040506070809000102030405060708090102"))
    assert(pr.timestamp == 1525250535L)
    assert(pr.nodeId == PublicKey(BinaryData("03e7156ae33b0a208d0744199163177e909e80176e55d97a2f221ede0f934dd9ad")))
    assert(pr.description == Left("One piece of chocolate cake, one icecream cone, one pickle"))
    assert(pr.fallbackAddress === Some("58B2XNFHncXtNFj1bpgKtiP8H1hirQpXBy"))
    assert(pr.routingInfo === List(List(
      ExtraHop(PublicKey("029e03a901b85534ff1e92c43c74431f7ce72046060fcf7a95c37e148f78c77255"), ShortChannelId(1), 10, 11, 12),
      ExtraHop(PublicKey("039e03a901b85534ff1e92c43c74431f7ce72046060fcf7a95c37e148f78c77255"), ShortChannelId(2), 10, 11, 12)
    )))
    assert(BinaryData(Protocol.writeUInt64(0x0102030405060708L, ByteOrder.BIG_ENDIAN)) == BinaryData("0102030405060708"))
    assert(BinaryData(Protocol.writeUInt64(0x030405060708090aL, ByteOrder.BIG_ENDIAN)) == BinaryData("030405060708090a"))
    assert(pr.tags.size == 4)
    assert(PaymentRequest.write(pr.sign(priv)) == ref)
  }

  test("expiry is a variable-length unsigned value") {
    val pr = PaymentRequest(Block.BCARegtestForkBlockHash, Some(MilliSatoshi(100000L)), BinaryData("0001020304050607080900010203040506070809000102030405060708090102"),
      priv, "test", fallbackAddress = None, expirySeconds = Some(21600), timestamp = System.currentTimeMillis() / 1000L)

    val serialized = PaymentRequest write pr
    val pr1 = PaymentRequest read serialized
    assert(pr.expiry === Some(21600))
  }

  test("ignore unknown tags") {
    // create a new tag that we don't know about
    class MyExpiryTag(override val seconds: Long) extends ExpiryTag(seconds) {
      // replace the tag with 'j'  which is not used yet
      override def toInt5s = super.toInt5s.updated(0, Bech32.map('j'))
    }

    val pr = PaymentRequest(
      prefix = "lntbca",
      amount = Some(MilliSatoshi(100000L)),
      timestamp = System.currentTimeMillis() / 1000L,
      nodeId = nodeId,
      tags = List(
        PaymentHashTag(BinaryData("01" * 32)),
        DescriptionTag("description"),
        new MyExpiryTag(42L)
      ),
      signature = BinaryData.empty).sign(priv)

    val serialized = PaymentRequest write pr
    val pr1 = PaymentRequest read serialized
    val Some(unknownTag) = pr1.tags.collectFirst { case u: UnknownTag => u }
    assert(unknownTag.tag == Bech32.map('j'))
    assert(unknownTag.toInt5s == (new MyExpiryTag(42L)).toInt5s)
  }
}
