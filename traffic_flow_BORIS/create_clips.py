import os
import subprocess

# Directory containing the videos
video_dir = "Lon_lon_NM23074_nest-emigration_230607"

# Output directory for clips
output_dir = os.path.join(video_dir, "flow")
os.makedirs(output_dir, exist_ok=True)

# Base filename pattern
base_name = "Lon_lon_NM23074_nest-emigration_230607"

# Loop through video segments
for i in range(16):  # 00 to 15
    video_file = f"{base_name}_{i:02d}.mp4"
    video_path = os.path.join(video_dir, video_file)

    # Create 3 clips: at 0:00, 10:00, 20:00
    for j in range(3):
        start_time = j * 10 * 60  # seconds
        clip_name = f"{base_name}_{i:02d}_clip_{j}.mp4"
        clip_path = os.path.join(output_dir, clip_name)

        cmd = [
            "ffmpeg",
            "-ss", str(start_time),
            "-i", video_path,
            "-t", "60",
            "-vf", "drawbox=x=iw/2:y=0:w=1:h=ih:color=white@0.8:t=fill",
            "-c:v", "libx264",
            "-crf", "23",
            "-preset", "veryfast",
            "-c:a", "copy",
            clip_path
        ]
        print(f"Extracting: {clip_path}")
        subprocess.run(cmd, check=True)

print("Done.")
