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

package fr.acinq.eclair

import fr.acinq.bitcoin.Crypto.PrivateKey
import fr.acinq.bitcoin.{Base58, Base58Check, Bech32, BinaryData, Block, Crypto, Script}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.util.Try

/**
  * Created by PM on 27/01/2017.
  */
@RunWith(classOf[JUnitRunner])
class PackageSpec extends FunSuite {

  test("compute long channel id") {
    val data = (BinaryData("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"), 0, BinaryData("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")) ::
      (BinaryData("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"), 1, BinaryData("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE")) ::
      (BinaryData("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000"), 2, BinaryData("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0002")) ::
      (BinaryData("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00F0"), 0x0F00, BinaryData("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0FF0")) :: Nil

    data.foreach(x => assert(toLongId(x._1, x._2) === x._3))
  }

  test("decode base58 addresses") {
    val priv = PrivateKey(BinaryData("01" * 32), compressed = true)
    val pub = priv.publicKey

    // p2pkh
    // valid chain
    assert(addressToPublicKeyScript(Base58Check.encode(Base58.Prefix.PubkeyAddressTestnet, pub.hash160), Block.BCATestnetForkBlockHash) == Script.pay2pkh(pub))
    assert(addressToPublicKeyScript(Base58Check.encode(Base58.Prefix.PubkeyAddressTestnet, pub.hash160), Block.BCARegtestForkBlockHash) == Script.pay2pkh(pub))
    assert(addressToPublicKeyScript(Base58Check.encode(Base58.Prefix.PubkeyAddress, pub.hash160), Block.BCALivenetForkBlockHash) == Script.pay2pkh(pub))

    // wrong chain
    intercept[RuntimeException] {
      addressToPublicKeyScript(Base58Check.encode(Base58.Prefix.PubkeyAddress, pub.hash160), Block.BCATestnetForkBlockHash)
    }
    assert(Try(addressToPublicKeyScript(Base58Check.encode(Base58.Prefix.PubkeyAddress, pub.hash160), Block.BCATestnetForkBlockHash)).isFailure)
    assert(Try(addressToPublicKeyScript(Base58Check.encode(Base58.Prefix.PubkeyAddress, pub.hash160), Block.BCARegtestForkBlockHash)).isFailure)

    // p2sh
    val script = Script.write(Script.pay2wpkh(pub))

    // valid chain
    assert(addressToPublicKeyScript(Base58Check.encode(Base58.Prefix.ScriptAddressTestnet, Crypto.hash160(script)), Block.BCATestnetForkBlockHash) == Script.pay2sh(script))
    assert(addressToPublicKeyScript(Base58Check.encode(Base58.Prefix.ScriptAddressTestnet, Crypto.hash160(script)), Block.BCARegtestForkBlockHash) == Script.pay2sh(script))
    assert(addressToPublicKeyScript(Base58Check.encode(Base58.Prefix.ScriptAddress, Crypto.hash160(script)), Block.BCALivenetForkBlockHash) == Script.pay2sh(script))

    // wrong chain
    assert(Try(addressToPublicKeyScript(Base58Check.encode(Base58.Prefix.ScriptAddressTestnet, Crypto.hash160(script)), Block.BCALivenetForkBlockHash)).isFailure)
    assert(Try(addressToPublicKeyScript(Base58Check.encode(Base58.Prefix.ScriptAddress, Crypto.hash160(script)), Block.BCATestnetForkBlockHash)).isFailure)
    assert(Try(addressToPublicKeyScript(Base58Check.encode(Base58.Prefix.ScriptAddress, Crypto.hash160(script)), Block.BCARegtestForkBlockHash)).isFailure)
  }

  test("decode bech32 addresses") {
    val priv = PrivateKey(BinaryData("01" * 32), compressed = true)
    val pub = priv.publicKey

    // p2wpkh
    assert(addressToPublicKeyScript(Bech32.encodeWitnessAddress("bca", 0, pub.hash160), Block.BCALivenetForkBlockHash) == Script.pay2wpkh(pub))
    assert(addressToPublicKeyScript(Bech32.encodeWitnessAddress("tbca", 0, pub.hash160), Block.BCATestnetForkBlockHash) == Script.pay2wpkh(pub))
    assert(addressToPublicKeyScript(Bech32.encodeWitnessAddress("bcart", 0, pub.hash160), Block.BCARegtestForkBlockHash) == Script.pay2wpkh(pub))

    // wrong version
    assert(Try(addressToPublicKeyScript(Bech32.encodeWitnessAddress("bca", 1, pub.hash160), Block.BCALivenetForkBlockHash)).isFailure)
    assert(Try(addressToPublicKeyScript(Bech32.encodeWitnessAddress("tbca", 1, pub.hash160), Block.BCATestnetForkBlockHash)).isFailure)
    assert(Try(addressToPublicKeyScript(Bech32.encodeWitnessAddress("bcart", 1, pub.hash160), Block.BCARegtestForkBlockHash)).isFailure)

    // wrong chain
    assert(Try(addressToPublicKeyScript(Bech32.encodeWitnessAddress("bca", 0, pub.hash160), Block.BCATestnetForkBlockHash)).isFailure)
    assert(Try(addressToPublicKeyScript(Bech32.encodeWitnessAddress("tbca", 0, pub.hash160), Block.BCALivenetForkBlockHash)).isFailure)
    assert(Try(addressToPublicKeyScript(Bech32.encodeWitnessAddress("bcart", 0, pub.hash160), Block.BCALivenetForkBlockHash)).isFailure)

    val script = Script.write(Script.pay2wpkh(pub))
    assert(addressToPublicKeyScript(Bech32.encodeWitnessAddress("bca", 0, Crypto.sha256(script)), Block.BCALivenetForkBlockHash) == Script.pay2wsh(script))
    assert(addressToPublicKeyScript(Bech32.encodeWitnessAddress("tbca", 0, Crypto.sha256(script)), Block.BCATestnetForkBlockHash) == Script.pay2wsh(script))
    assert(addressToPublicKeyScript(Bech32.encodeWitnessAddress("bcart", 0, Crypto.sha256(script)), Block.BCARegtestForkBlockHash) == Script.pay2wsh(script))
  }

  test("fail to decode invalid addresses") {
    val e = intercept[RuntimeException] {
      addressToPublicKeyScript("1Qbbbbb", Block.BCALivenetForkBlockHash)
    }
    assert(e.getMessage.contains("is neither a valid Base58 address") && e.getMessage.contains("nor a valid Bech32 address"))
  }

  test("convert fee rates and enforce a minimum feerate-per-kw") {
    assert(feerateByte2Kw(1) == MinimumFeeratePerKw)
    assert(feerateKB2Kw(1000) == MinimumFeeratePerKw)
  }

  test("compare short channel ids as unsigned longs") {
    assert(ShortChannelId(Long.MinValue - 1) < ShortChannelId(Long.MinValue))
    assert(ShortChannelId(Long.MinValue) < ShortChannelId(Long.MinValue + 1))
    assert(ShortChannelId(Long.MaxValue - 1) < ShortChannelId(Long.MaxValue))
    assert(ShortChannelId(Long.MaxValue) < ShortChannelId(Long.MaxValue + 1))
  }
}
