/*
 * Copyright (c) 2022 Lyn
 * Skew is licensed under Mulan PubL v2.
 * You can use this software according to the terms and conditions of the Mulan PubL v2.
 * You may obtain a copy of Mulan PubL v2 at:
 *         http://license.coscl.org.cn/MulanPubL-2.0
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 * See the Mulan PubL v2 for more details.
 */
package matrix
import freechips.rocketchip.config.{Config, Parameters}
import matrix.common._

object MatrixTestUnit {
  private def augment(tp: MatrixTileParams)(implicit p: Parameters): Parameters = p.alterPartial {
    case MatrixTileKey => tp
  }
  def getMatrixParameters(configName: String, configPackage: String = "matrix.system"): Parameters = {
    val fullConfigName = configPackage + "." + configName
    val origParams: Parameters = try {
      Class.forName(fullConfigName).newInstance().asInstanceOf[Config] ++ Parameters.empty
    } catch {
      case e: java.lang.ClassNotFoundException =>
        throw new Exception(s"""Unable to find config $fullConfigName""", e)
    }
    val matrixTileParams = origParams(MatrixTileKey)
    val outParams = augment(matrixTileParams)(origParams)
    outParams
  }
}