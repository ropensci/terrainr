using UnityEngine;
using UnityEditor;
using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;

public class ManifestImport : EditorWindow {
    private static EditorWindow window;
    private string manifestPath = "terrainr.manifest";

    public enum Depth { Bit8 = 1, Bit16 = 2 }
    public enum ByteOrder { Mac = 1, Windows = 2 }
    public Depth m_Depth = Depth.Bit16;
    public int m_Resolution = 1;
    public ByteOrder m_ByteOrder = ByteOrder.Windows;

    [MenuItem("terrainr/Import from manifest")]
    public static void CreateWindow(){
        window = EditorWindow.GetWindow(typeof(ManifestImport));
        window.title = "Manifest import";
    }

    private void OnGUI(){
        manifestPath  = EditorGUILayout.TextField("Path to manifest: ", manifestPath);
        
        if(GUILayout.Button("Import")){
            using (StreamReader reader = new StreamReader(manifestPath))
            {
                string line;
                while ((line = reader.ReadLine()) != null)
                {
                    string[] fields = line.Split('\t');
                    string heightmapPath = fields[0];
                    float x_pos = float.Parse(fields[1]);
                    float z_pos = float.Parse(fields[2]);
                    float width = float.Parse(fields[3]);
                    float height = float.Parse(fields[4]);
                    float length = float.Parse(fields[5]);
                    int heightmapResolution = int.Parse(fields[6]);
                    string texturePath = fields[7];

                    CreateTerrain(heightmapPath, x_pos, z_pos, width, height, length, heightmapResolution, texturePath);

                    this.Close();
                }
            }
        }
    }
    
    private void ValidatePath(string heightmapPath){
        if(File.Exists(heightmapPath) == false){
            throw new ArgumentException("Could not find file: " + heightmapPath);
        }
    }
    
    private void CreateTerrain(string t_heightmapPath,
                               float t_x_pos,
                               float t_z_pos,
                               float t_width, 
                               float t_height, 
                               float t_length,
                               int t_heightmapResolution,
                               string t_texturePath){
        ValidatePath(t_heightmapPath);
        TerrainData terrainData = new TerrainData();
        terrainData.size = new Vector3(t_width / 128, t_height, t_length / 128);
        terrainData.heightmapResolution = t_heightmapResolution;

        GameObject terrain = (GameObject)Terrain.CreateTerrainGameObject(terrainData);
        terrain.transform.position = new Vector3(t_x_pos, 0, t_z_pos);
        ReadRaw(t_heightmapPath, terrainData);
        if(t_texturePath != string.Empty){
            AddTexture(t_texturePath, terrainData, t_width, t_length);
        }
        AssetDatabase.CreateAsset(terrainData, "Assets/" + t_heightmapPath + ".asset");
    }

    void ReadRaw(string path, TerrainData terrainData)
    {
        m_Resolution = terrainData.heightmapResolution;

        // Read data
        byte[] data;
        using (BinaryReader br = new BinaryReader(File.Open(path, FileMode.Open, FileAccess.Read)))
        {
            data = br.ReadBytes(m_Resolution * m_Resolution * (int)m_Depth);
            br.Close();
        }

        int heightmapRes = terrainData.heightmapResolution;
        float[,] heights = new float[heightmapRes, heightmapRes];

            float normalize = 1.0F / (1 << 16);
            for (int y = 0; y < heightmapRes; ++y)
            {
                for (int x = 0; x < heightmapRes; ++x)
                {
                    int index = Mathf.Clamp(x, 0, m_Resolution - 1) + Mathf.Clamp(y, 0, m_Resolution - 1) * m_Resolution;
                    ushort compressedHeight = System.BitConverter.ToUInt16(data, index * 2);
                    float height = compressedHeight * normalize;
                    heights[y, x] = height;
                }
            }

        terrainData.SetHeights(0, 0, heights);
    }

    void AddTexture(string path, TerrainData terrainData, float t_width, float t_length)
    {
        Texture2D texture = LoadPNG(path);
        AssetDatabase.CreateAsset(texture, "Assets/texture_" + path + ".asset");
        TerrainLayer overlay = new TerrainLayer();
        overlay.tileSize = new Vector2(t_width, t_length);
        overlay.diffuseTexture = texture;
        AssetDatabase.CreateAsset(overlay, "Assets/overlay_" + path + ".asset");
        var layers = terrainData.terrainLayers;
        int newIndex = layers.Length;
        var newarray = new TerrainLayer[newIndex + 1];
        Array.Copy(layers, 0, newarray, 0, newIndex);
        newarray[newIndex] = overlay;
        terrainData.SetTerrainLayersRegisterUndo(newarray, "Add terrain layer");
    }

    private static Texture2D LoadPNG(string imgPath){
        Texture2D texture = null;
        byte[] imgData;

        imgData = File.ReadAllBytes(imgPath);
        texture = new Texture2D(2, 2);
        texture.LoadImage(imgData);

        return texture;
    }
}
