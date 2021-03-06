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

package fr.acinq.eclair.wire

import fr.acinq.bitcoin.{BinaryData, Block}
import fr.acinq.eclair.ShortChannelId
import org.scalatest.FunSuite
import scodec.bits.BitVector

import scala.util.Random

/**
  * Created by PM on 31/05/2016.
  */

class FailureMessageCodecsSpec extends FunSuite {
  val channelUpdate = ChannelUpdate(
    signature = BinaryData("3045022100c451cd65c88f55b1767941a247e849e12f5f4d4a93a07316659e22f5267d2088022009042a595c6bc8942cd9d729317b82b306edc259fb6b3a3cecb3dd1bd446e90641"),
    chainHash = Block.BCARegtestForkBlockHash,
    shortChannelId = ShortChannelId(12345),
    timestamp = 1234567L,
    cltvExpiryDelta = 100,
    messageFlags = 0,
    channelFlags = 1,
    htlcMinimumMsat = 1000,
    feeBaseMsat = 12,
    feeProportionalMillionths = 76,
    htlcMaximumMsat = None)

  def randomBytes(size: Int): BinaryData = {
    val bin = new Array[Byte](size)
    Random.nextBytes(bin)
    bin
  }

  test("encode/decode all channel messages") {
    val msgs: List[FailureMessage] =
      InvalidRealm :: TemporaryNodeFailure :: PermanentNodeFailure :: RequiredNodeFeatureMissing ::
        InvalidOnionVersion(randomBytes(32)) :: InvalidOnionHmac(randomBytes(32)) :: InvalidOnionKey(randomBytes(32)) ::
        TemporaryChannelFailure(channelUpdate) :: PermanentChannelFailure :: RequiredChannelFeatureMissing :: UnknownNextPeer ::
        AmountBelowMinimum(123456, channelUpdate) :: FeeInsufficient(546463, channelUpdate) :: IncorrectCltvExpiry(1211, channelUpdate) :: ExpiryTooSoon(channelUpdate) ::
        UnknownPaymentHash :: IncorrectPaymentAmount :: FinalExpiryTooSoon :: FinalIncorrectCltvExpiry(1234) :: ChannelDisabled(0, 1, channelUpdate) :: ExpiryTooFar :: Nil

    msgs.foreach {
      case msg => {
        val encoded = FailureMessageCodecs.failureMessageCodec.encode(msg).require
        val decoded = FailureMessageCodecs.failureMessageCodec.decode(encoded).require
        assert(msg === decoded.value)
      }
    }
  }
}
